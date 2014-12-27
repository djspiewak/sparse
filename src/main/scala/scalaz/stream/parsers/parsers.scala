package scalaz.stream
package parsers

import scalaz._
import scalaz.syntax.equal._
import scalaz.syntax.monad._
import scalaz.syntax.show._

sealed trait Parser[Token, Result] {

  def map[Result2](f: Result => Result2): Parser[Token, Result2]
}

object Parser {

  // yep, indexing on value identity LIKE A BOSS
  type Cache[Token, Result] = Map[(Token, Parser[Token, Result]), Parser[Token, Result]]

  // it's somewhat important that these functions be lazy
  implicit class RichParser[Token, Result](left: => Parser[Token, Result]) {

    // alias for andThen
    def ~[Result2](right: => Parser[Token, Result2]) = andThen(right)

    def andThen[Result2](right: => Parser[Token, Result2]): Parser[Token, Result ~ Result2] =
      new SeqParser(left, right)

    // alias for orElse
    def |(right: => Parser[Token, Result]) = orElse(right)

    def orElse(right: => Parser[Token, Result]): Parser[Token, Result] =
      new UnionParser(left, right)
  }

  def completed[Token, Result](r: Result): Parser[Token, Result] = Completed(r)
  def error[Token, Result](msg: String): Parser[Token, Result] = Error(msg)

  def literal[Token: Equal: Show](token: Token): Parser[Token, Token] = new Incomplete[Token, Token] {

    def innerRun(seen: Set[Parser[Token, Token]]) = \/-(Error(s"unexpected end of stream; expected ${token.shows}"))

    def innerDerive(candidate: Token) = {
      val result: Parser[Token, Token] = if (candidate === token)
        completed(token)
      else
        error(s"expected ${token.shows}, got ${candidate.shows}")

      State state result
    }
  }

  //
  // algebra
  //

  // note that this is *not* a NEL; we're going to forbid global ambiguity for now
  final case class Completed[Token, Result](result: Result) extends Parser[Token, Result] {
    def map[Result2](f: Result => Result2) = Completed(f(result))
  }

  // yep!  it's a string.  deal with it
  final case class Error[Token, Result](msg: String) extends Parser[Token, Result] {
    def map[Result2](f: Result => Result2) = Error(msg)
  }

  sealed trait Incomplete[Token, Result] extends Parser[Token, Result] { outer =>

    def map[Result2](f: Result => Result2) = new Incomplete[Token, Result2] {

      def innerRun(seen: Set[Parser[Token, Result]]) = outer.run(seen).bimap(_ map f, _ map f)

      def innerDerive(candidate: Token) = for {
        derivative <- outer innerDerive candidate
        cache <- State.get[Cache[Token, Result]]

        cache2 = cache map {
          case (key, value) => key -> (value map f)
        }

        _ <- State.iPut(cache2)
      } yield derivative map f
    }

    final def run(seen: Set[Parser[Token, Result]]): Completed[Token, Result] \/ Error[Token, Result] = {
      // as a side note, this comparison being on pointer identity is the reason this algorithm is O(k^n)
      // if we could magically compare parsers on equivalence of the language they generate, the algorithm
      // would be O(n^2), even if I reenabled global ambiguity support.  SO CLOSE!
      if (seen contains this)
        \/-(Error("divergent"))
      else
        innerRun(seen + this)
    }

    protected def innerRun(seen: Set[Parser[Token, Result]]): Completed[Token, Result] \/ Error[Token, Result]

    // progress the parse over one token
    final def derive(t: Token): State[Cache[Token, Result], Parser[Token, Result]] = for {
      cache <- State.get[Cache[Token, Result]]
      back <- cache get (t -> this) map { State.state[Cache[Token, Result], Parser[Token, Result]](_) } getOrElse innerDerive(candidate)

      cache2 = cache + ((t, this) -> back)
      _ <- State.put(cache2)
    } yield back

    protected def innerDerive(candidate: Token): State[Cache[Token, Result], Parser[Token, Result]]
  }
}

private[parsers] class SeqParser[Token, LR, RR](_left: => Parser[Token, LR], _right: Parser[Token, RR]) extends Parser.Incomplete[Token, LR ~ RR] {
  import Parser._

  private lazy val left = _left
  private lazy val right = _right

  def innerRun(seen: Set[Parser[Token, Result]]): Completed[Token, LR ~ RR] \/ Error[Token, LR ~ RR] = for {
    lr <- left run seen
    rr <- right run seen
  } yield ~(lr, rr)

  def innerDerive(t: Token): State[Cache[Token, Result], Parser[Token, LR ~ RR]] = left match {
    case Completed(lr) => for {
      rp <- right derive t
    } yield rp map { ~(lr, _) }

    case e @ Error(_) => e.point

    case left: Incomplete[Token, LR] => {
      left.run(Set()).toOption map { lr =>
        for {
          lp <- left derive t
          rp <- right derive t
        } yield lp ~ right | (rp map { ~(lr, _) })
      } getOrElse {
        for {
          lp <- left derive t
        } yield lp ~ right
      }
    }
  }
}

private[parsers] class UnionParser[Token, Result](_left: => Parser[Token, Result], _right: Parser[Token, Result]) extends Parser.Incomplete[Token, Result] {
  import Parser._

  private lazy val left = _left
  private lazy val right = _right

  def innerRun(seen: Set[Parser[Token, Result]]): Completed[Token, Result] \/ Error[Token, Result] = {
    (left run seen, right run seen) match {
      case (-\/(Completed(_)), -\/(Completed(_))) => \/-(Error("global ambiguity detected"))
      case (lr @ -\/(Completed(_)), \/-(Error(_))) => lr
      case (\/-(Error(_)), rr @ -\/(Completed(_))) => rr
      case (\/-(Error(msg)), \/-(Error(msg2))) => {
        if (msg === msg2)
          \/-(Error(msg))
        else
          \/-(Error(s"$msg and $msg2"))
      }
    }
  }

  def innerDerive(t: Token): State[Cache[Token, Result], Parser[Token, Result]] = (left, right) match {
    case (Error(leftMsg), Error(rightMsg)) => State.state(error(s"$leftMsg OR $rightMsg"))
    case (Error(_), right) => right derive t
    case (left, Error(_)) => left derive t

    case (left, right) => for {
      lp <- left derive t
      rp <- right derive t
    } yield lp | rp         // TODO this value needs to be in the Cache[Token, Result] *before* we recurse
  }
}
