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
