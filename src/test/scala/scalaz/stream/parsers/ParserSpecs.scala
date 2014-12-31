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

import org.specs2.matcher.Matcher
import org.specs2.mutable._

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.string._
import scalaz.syntax.equal._

import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.StringOps

import scala.util.matching.Regex

object ParserSpecs extends Specification {
  import Parser.{Error, Completed, literalRichParser}

  "terminal parsers" should {
    "parse the empty string" in {
      val epsilon: Parser[Char, Unit] = Parser.completed(())
      epsilon must parseComplete("").as(())
    }

    "parse a single token" in {
      val a: Parser[Char, Char] = 'a'
      a must parseComplete("a").as('a')
    }

    "produce an error when so defined" in {
      val e: Parser[Char, Unit] = Parser.error("oogly boogly")
      e must parseError("fubar").as("oogly boogly")
    }
  }

  "parentheses matching" should {
    lazy val grammar: Parser[Char, Int] = (
        '(' ~> grammar <~ ')' ^^ (1 +)
      | Parser.completed(0)
    )

    "parse the empty string" in {
      grammar must parseComplete("").as(0)
    }

    "parse a single set of parentheses" in {
      grammar must parseComplete("()").as(1)
    }

    "parse four nested sets of parentheses" in {
      grammar must parseComplete("(((())))").as(4)
    }

    "fail to parse a single mismatched paren" in {
      grammar must parseError("(").as("unexpected end of stream; expected )")
    }

    "fail to parse three mismatched parens with one match" in {
      grammar must parseError("(((()").as("unexpected end of stream; expected )")
    }

    "fail to parse a mismatched closing paren" in {
      grammar must parseError(")").as("expected (, got )")
    }
  }

  "an expression evaluator" should {
    sealed trait ExprToken

    object ExprToken {
      final case class Num(n: Int) extends ExprToken

      case object Plus extends ExprToken
      case object Minus extends ExprToken
      case object Times extends ExprToken
      case object Div extends ExprToken

      case object LParen extends ExprToken
      case object RParen extends ExprToken
    }

    implicit def exprTokenEq[T <: ExprToken]: Equal[T] = Equal.equalA      // because I'm lazy
    implicit def exprTokenShow[T <: ExprToken]: Show[T] = Show.showA       // ditto!

    import ExprToken._

    val rules: Map[Regex, List[String] => ExprToken] = Map(
      """\s*(\d+)""".r -> { case ns :: Nil => Num(ns.toInt) },

      """\s*\+""".r -> { _ => Plus },
      """\s*-""".r -> { _ => Minus },
      """\s*\*""".r -> { _ => Times },
      """\s*/""".r -> { _ => Div },

      """\s*\(""".r -> { _ => LParen },
      """\s*\)""".r -> { _ => RParen })

    def exprTokenize(str: String): Seq[ExprToken] =
      regexTokenize(str, rules) collect { case \/-(et) => et }

    // %%

    lazy val expr: Parser[ExprToken, Int] = (
        expr ~ Plus ~ term  ^^ { (e1, _, e2) => e1 + e2 }
      | expr ~ Minus ~ term ^^ { (e1, _, e2) => e1 - e2 }
      | term
    )

    lazy val term: Parser[ExprToken, Int] = (
        term ~ Times ~ value ^^ { (e1, _, e2) => e1 * e2 }
      | term ~ Div ~ value   ^^ { (e1, _, e2) => e1 / e2 }
      | value
    )

    // type inference and invariance sort of failed me here...
    lazy val value: Parser[ExprToken, Int] = (
        (LParen: Parser[ExprToken, ExprToken]) ~> expr <~ RParen
      | (Parser pattern { case Num(n) => n })
    )

    // %%

    "tokenize a number" in {
      exprTokenize("42") mustEqual Seq(Num(42))
    }

    "parse a number" in {
      expr must parseComplete(exprTokenize("42")).as(42)
      expr must parseComplete(exprTokenize("12")).as(12)
    }

    "parse a simple addition expression" in {
      expr must parseComplete(exprTokenize("1 + 2")).as(3)
    }

    "parse a complex composition of all four operators" in {
      expr must parseComplete(exprTokenize("228 * 4 + 12")).as(924)
      expr must parseComplete(exprTokenize("123 + 228 * 4 + 12")).as(1047)
      expr must parseComplete(exprTokenize("123 - 2 + 228 * 4 + 12")).as(1045)
      expr must parseComplete(exprTokenize("123 - 2 + 228 * 4 + 12 / 4 + 79")).as(1115)
      expr must parseComplete(exprTokenize("123 - 2 + 228 * 4 + 12 / 4 + 79 * 5")).as(1431)
    }

    // TODO more expr tests
  }

  // TODO maybe move this to a Util object?  seems useful
  def parse[T, R](parser: Parser[T, R])(str: Seq[T]): Error[T, R] \/ Completed[T, R] = {
    def inner(str: Seq[T])(parser: Parser[T, R]): State[Parser.Cache[T], Error[T, R] \/ Completed[T, R]] = {
      if (str.isEmpty) {
        State state parser.complete()
      } else {
        parser match {
          case Completed(_) => State state -\/(Error("unexpected end of stream"))
          case e @ Error(_) => State state -\/(e)

          case parser: Parser.Incomplete[T, R] =>
            parser derive str.head flatMap inner(str.tail)
        }
      }
    }

    inner(str)(parser) eval Parser.Cache[T]
  }

  // TODO this also seems useful...
  def tokenize[Str[_] <: SeqLike[_, _], TokenIn, TokenOut, That <: TraversableOnce[TokenIn \/ TokenOut]](str: Str[TokenIn])(f: Str[TokenIn] => (TokenIn \/ TokenOut, Str[TokenIn]))(implicit cbf: CanBuildFrom[Str[TokenIn], TokenIn \/ TokenOut, That]): That = {
    if (str.isEmpty) {
      cbf().result
    } else {
      val (token, tail) = f(str)

      val builder = cbf()
      builder += token
      builder ++= tokenize(tail)(f)      // TODO it's never worse, tail-recurse!
      builder.result
    }
  }

  // TODO oh look, more useful stuff!
  def regexTokenize[T](str: String, rules: Map[Regex, List[String] => T]): Seq[Char \/ T] = {
    def iseqAsCharSeq(seq: IndexedSeq[Char]): CharSequence = new CharSequence {
      def charAt(i: Int) = seq(i)
      def length = seq.length
      def subSequence(start: Int, end: Int) = iseqAsCharSeq(seq.slice(start, end))
      override def toString = seq.mkString
    }

    tokenize(str: IndexedSeq[Char]) { seq =>
      val str = iseqAsCharSeq(seq)

      // find the "first" regex that matches and apply its transform
      val tokenM: Option[(T, IndexedSeq[Char])] = rules collectFirst {
        case (regex, f) if (regex findPrefixMatchOf str).isDefined => {
          val m = (regex findPrefixMatchOf str).get
          (f(m.subgroups), m.after.toString: IndexedSeq[Char])
        }
      }

      tokenM map {
        case (token, tail) => (\/-(token), tail)
      } getOrElse ((-\/(seq.head), seq.tail))
    }
  }

  //
  // custom matchers
  //

  def parseComplete[T](str: Seq[T]) = new {
    def as[R: Equal](result: R): Matcher[Parser[T, R]] = {
      def body(parser: Parser[T, R]) = {
        parse(parser)(str) match {
          case \/-(Completed(r)) => r === result
          case -\/(_) => false
        }
      }

      def error(parser: Parser[T, R]) = parse(parser)(str) match {
        case -\/(Error(str)) => s"produces error: $str"
        case \/-(Completed(r)) => s"produces result $r rather than expected $result"
      }

      (body _,
        Function.const("parses successfully") _,
        error _)
    }
  }

  def parseError[T](str: Seq[T]) = new {
    def as[R](msg: String): Matcher[Parser[T, R]] = {
      def body(parser: Parser[T, R]) = {
        parse(parser)(str) match {
          case \/-(Completed(r)) => false
          case -\/(Error(msg2)) => msg === msg2
        }
      }

      def error(parser: Parser[T, R]) = parse(parser)(str) match {
        case -\/(Error(msg2)) => s"produced error '$msg2' and not '$msg'"
        case \/-(_) => "completed and did not error"
      }

      (body _,
        Function.const(s"produces error $msg") _,
        error _)
    }
  }
}
