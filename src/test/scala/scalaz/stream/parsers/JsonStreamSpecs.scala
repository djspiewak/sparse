/*
 * Copyright 2015 Daniel Spiewak
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package scalaz.stream
package parsers

import org.specs2.mutable._

import scalaz._

import scala.util.matching.Regex

/**
 * An example playground which stream parses characters into JSON.
 */
object JsonStreamSpecs extends Specification {

  "json lexing" should {
    "lex an empty object literal" in {
      val output = Process("{}": _*).toSource pipe tokenize(JsonToken.Rules) stripW

      output.runLog.run mustEqual Seq(JsonToken.LBrace, JsonToken.RBrace)
    }
  }

  "json parsing" should {
    "parse the fundamental values" in {
      "object" >> {
        val output = (Process("{}": _*).toSource pipe tokenize(JsonToken.Rules) stripW) pipe parse(value) stripW

        output.runLog.run mustEqual Seq(JsonValue.Object(Vector.empty))
      }
    }

    "parse an unmatched object literal without SOE" in {
      val output = Process(JsonToken.LBrace, JsonToken.LBrace, JsonToken.RBrace).toSource pipe parse(value) stripW

      output.runLog.run must not(throwA[StackOverflowError])
    }
  }

  sealed trait JsonToken {
    final val as: JsonToken = this     // lightweight ascription
  }

  object JsonToken {
    case object LBrace extends JsonToken    // {
    case object RBrace extends JsonToken    // }

    case object LBracket extends JsonToken  // [
    case object RBracket extends JsonToken  // ]

    case object Comma extends JsonToken   // ,
    case object Colon extends JsonToken   // :

    final case class Str(str: String) extends JsonToken     // "foo"
    final case class Num(value: Double) extends JsonToken   // 3.14

    case object True extends JsonToken    // true
    case object False extends JsonToken   // false

    val Rules: Map[Regex, PartialFunction[List[String], JsonToken]] = Map(
      """\{""".r -> { case Nil => LBrace },
      """\}""".r -> { case Nil => RBrace },

      """\[""".r -> { case Nil => LBracket },
      """\]""".r -> { case Nil => RBracket },

      """,""".r -> { case Nil => Comma },
      """:""".r -> { case Nil => Colon },

      """"(([^"]|\\.)+)"""".r -> { case body :: Nil => Str(canonicalizeStr(body)) },
      """(\d+(\.\d+)?)""".r -> { case body :: Nil => Num(body.toDouble) },

      "true".r -> { case Nil => True },
      "false".r -> { case Nil => False })

    private def canonicalizeStr(body: String): String = {
      val (_, back) = body.foldLeft((false, "")) {
        case ((true, acc), c) => (false, acc + c)
        case ((false, acc), '\\') => (true, acc)
        case ((false, acc), c) => (false, acc + c)
      }

      back
    }

    implicit val eq: Equal[JsonToken] = Equal.equalA[JsonToken]
    implicit val show: Show[JsonToken] = Show.showA[JsonToken]
  }

  sealed trait JsonValue {
    final val as: JsonValue = this
  }

  object JsonValue {
    final case class Object(fields: Vector[(String, JsonValue)]) extends JsonValue
    final case class Array(values: Vector[JsonValue]) extends JsonValue

    final case class Str(str: String) extends JsonValue
    final case class Num(value: Double) extends JsonValue

    case object True extends JsonValue
    case object False extends JsonValue
  }

  // %%

  import JsonToken._
  import Parser.{completed, pattern, literalRichParser}

  lazy val value: Parser[JsonToken, JsonValue] = (
      LBrace.as ~> fields <~ RBrace.as     ^^ JsonValue.Object ^^ { _.as }
    | LBracket.as ~> values <~ LBracket.as ^^ JsonValue.Array ^^ { _.as }

    | strValue
    | numValue
    | boolValue
  )

  // type inference went on vacation for this declaration, as you can see...
  lazy val fields: Parser[JsonToken, Vector[(String, JsonValue)]] = (
      ((fields ~ Comma.as ~ field) map { (pair: ((Vector[(String, JsonValue)], JsonToken), (String, JsonValue))) => pair._1._1 :+ pair._2 })
    | completed(Vector.empty)
  )

  lazy val field: Parser[JsonToken, (String, JsonValue)] =
    pattern[JsonToken, String]({ case Str(body) => body }) ~ Colon ~ value ^^ { (name, _, value) => (name, value) }

  lazy val values: Parser[JsonToken, Vector[JsonValue]] = (
      values ~ Comma ~ value ^^ { (vs, _, v) => vs :+ v }
    | completed(Vector.empty)
  )

  lazy val strValue: Parser[JsonToken, JsonValue] =
    pattern { case Str(body) => JsonValue.Str(body) }

  lazy val numValue: Parser[JsonToken, JsonValue] =
    pattern { case Num(value) => JsonValue.Num(value) }

  lazy val boolValue: Parser[JsonToken, JsonValue] = (
      True.as  ^^^ JsonValue.True.as
    | False.as ^^^ JsonValue.False.as
  )

  // %%
}
