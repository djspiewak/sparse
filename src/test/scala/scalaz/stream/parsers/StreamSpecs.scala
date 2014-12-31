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

import scalaz.std.anyVal._

object StreamSpecs extends Specification {
  import Parser.{completed, literalRichParser}

  "parentheses stream parsing" should {
    lazy val parens: Parser[Char, Int] = (
        '(' ~> parens <~ ')' ^^ (1 +)
      | completed(0)
    )

    "parse individual single parens" in {
      val result = Process("()": _*).toSource pipe parse(parens) stripW

      result.runLog.run mustEqual Seq(1, 0)
    }

    "parse multiple parens" in {
      val result = Process("()()()()()()()()()()()()()": _*).toSource pipe parse(parens) stripW

      result.runLog.run mustEqual Seq(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
    }

    "parse parens nested at arbitrary depth in sequence" in {
      val result = Process("((()))(())()((((()))))((()))()(((())))": _*).toSource pipe parse(parens) stripW

      result.runLog.run mustEqual Seq(3, 2, 1, 5, 3, 1, 4, 0)
    }
  }
}
