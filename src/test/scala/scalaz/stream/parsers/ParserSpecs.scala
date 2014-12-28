package scalaz.stream
package parsers

import org.specs2.matcher.Matcher
import org.specs2.mutable._

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.string._
import scalaz.syntax.equal._

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

  //
  // custom matchers
  //

  def parseComplete(str: String) = new {
    def as[R: Equal](result: R): Matcher[Parser[Char, R]] = {
      def body(parser: Parser[Char, R]) = {
        parse(parser)(str) match {
          case \/-(Completed(r)) => r === result
          case -\/(_) => false
        }
      }

      def error(parser: Parser[Char, R]) = parse(parser)(str) match {
        case -\/(Error(str)) => Some(str)
        case \/-(_) => None
      }

      (body _,
        Function.const("parses successfully") _,
        { str: Parser[Char, R] => s"produces error: ${error(str).get}" })
    }
  }

  def parseError(str: String) = new {
    def as[R](msg: String): Matcher[Parser[Char, R]] = {
      def body(parser: Parser[Char, R]) = {
        parse(parser)(str) match {
          case \/-(Completed(r)) => false
          case -\/(Error(msg2)) => msg === msg2
        }
      }

      def error(parser: Parser[Char, R]) = parse(parser)(str) match {
        case -\/(Error(msg2)) => s"produced error '$msg2' and not '$msg'"
        case \/-(_) => "completed and did not error"
      }

      (body _,
        Function.const(s"produces error $msg") _,
        error _)
    }
  }
}
