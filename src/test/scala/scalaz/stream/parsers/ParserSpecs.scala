package scalaz.stream
package parsers

import org.specs2.mutable._

import scalaz._
import scalaz.std.anyVal._

object ParserSpecs extends Specification {
  import Parser.{Error, Completed, literalRichParser}

  "terminal parsers" should {
    "parse the empty string" in {
      val epsilon: Parser[Char, Unit] = Parser.completed(())

      parse(epsilon)("") must beLike {
        case \/-(Completed(())) => ok
      }
    }

    "parse a single token" in {
      val a: Parser[Char, Char] = 'a'

      parse(a)("a") must beLike {
        case \/-(Completed(c)) => c mustEqual 'a'
      }
    }

    "produce an error when so defined" in {
      val e: Parser[Char, Unit] = Parser.error("oogly boogly")

      parse(e)("fubar") must beLike {
        case -\/(Error(str)) => str mustEqual "oogly boogly"
      }
    }
  }

  "parentheses matching" should {
    lazy val grammar: Parser[Char, Int] = (
        '(' ~> grammar <~ ')' ^^ (1 +)
      | Parser.completed(0)
    )

    "parse the empty string" in {
      parse(grammar)("") must beLike {
        case \/-(Completed(n)) => n mustEqual 0
      }
    }

    "parse a single set of parentheses" in {
      parse(grammar)("()") must beLike {
        case \/-(Completed(n)) => n mustEqual 1
      }
    }

    "parse four nested sets of parentheses" in {
      parse(grammar)("(((())))") must beLike {
        case \/-(Completed(n)) => n mustEqual 4
      }
    }
  }

  def parse[R](parser: Parser[Char, R])(str: String): Error[Char, R] \/ Completed[Char, R] = {
    def inner(str: String)(parser: Parser[Char, R]): State[Parser.Cache[Char], Error[Char, R] \/ Completed[Char, R]] = {
      if (str.isEmpty) {
        State state parser.complete()
      } else {
        parser match {
          case Completed(_) => State state -\/(Error("unexpected end of stream"))
          case e @ Error(_) => State state -\/(e)

          case parser: Parser.Incomplete[Char, R] =>
            parser derive str.head flatMap inner(str.tail)
        }
      }
    }

    inner(str)(parser) eval Parser.Cache[Char]
  }
}
