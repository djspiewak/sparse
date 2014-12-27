package scalaz.stream
package parsers

import scalaz._
import scalaz.syntax.equal._
import scalaz.syntax.monad._
import scalaz.syntax.show._

sealed trait Parser[Token, Result] {

  /**
   * Attempts to complete the parser, under the assumption that the stream has terminated. If the
   * parser contains a production for the empty string, it will complete and produce its result.
   * Otherwise, if no ε-production exists, an error will be produced.
   *
   * This function allows evaluators to request early termination on a possibly-unbounded incremental
   * parse.  For example, one might define a JSON grammar which parses an unbounded number of JSON
   * values, returning them as a list.  Such a grammar could complete early so long as the prefix
   * string of tokens defines a complete and self-contained JSON value.  This is a desirable property
   * for stream parsers, as it allows the evaluation to be driven (and halted) externally.
   *
   * Any parsers specified in the `seen` set will be treated as already traversed, indicating a cycle
   * in the graph.  Thus, if the traversal recursively reaches these parsers, that node will complete
   * to an error.  For a good time with the whole family, you can invoke `prsr.complete(Set(prsr))`,
   * which will produce an `Error("divergent")` for all non-trivial parsers (namely, parsers that
   * are not `Complete` or `Error` already).
   */
  def complete[R](seen: Set[Parser[Token, _]] = Set()): Parser.Error[Token, R] \/ Parser.Completed[Token, Result]

  /**
   * Parsers are functors, how 'bout that?  Note the lack of flatMap, though.  No context-sensitive
   * parsers allowed.
   */
  def map[Result2](f: Result => Result2): Parser[Token, Result2]
}

object Parser {

  // yep, indexing on value identity LIKE A BOSS
  type Cache[Token] = KMap[({ type λ[α] = (Token, Parser[Token, α]) })#λ, ({ type λ[α] = Parser[Token, α] })#λ]

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

    def innerComplete[R](seen: Set[Parser[Token, _]]) = -\/(Error(s"unexpected end of stream; expected ${token.shows}"))

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
    def complete[R](seen: Set[Parser[Token, _]]) = \/-(this)

    def map[Result2](f: Result => Result2) = Completed(f(result))
  }

  // yep!  it's a string.  deal with it
  final case class Error[Token, Result](msg: String) extends Parser[Token, Result] {
    def complete[R](seen: Set[Parser[Token, _]]) = -\/(Error(msg))

    def map[Result2](f: Result => Result2) = Error(msg)
  }

  object Error {
    implicit def monoid[Token, Result]: Monoid[Error[Token, Result]] = new Monoid[Error[Token, Result]] {

      def zero = Error("")

      def append(e1: Error[Token, Result], e2: => Error[Token, Result]): Error[Token, Result] =
        Error(s"${e1.msg} and ${e2.msg}")
    }
  }

  sealed trait Incomplete[Token, Result] extends Parser[Token, Result] { outer =>

    def map[Result2](f: Result => Result2) = new Incomplete[Token, Result2] {

      def innerComplete[R](seen: Set[Parser[Token, _]]) = outer.complete[R](seen).bimap(identity, _ map f)

      def innerDerive(candidate: Token) =
        outer innerDerive candidate map { _ map f }
    }

    final def complete[R](seen: Set[Parser[Token, _]]): Error[Token, R] \/ Completed[Token, Result] = {
      // as a side note, this comparison being on pointer identity is the reason this algorithm is O(k^n)
      // if we could magically compare parsers on equivalence of the language they generate, the algorithm
      // would be O(n^2), even if I reenabled global ambiguity support.  SO CLOSE!
      if (seen contains this)
        -\/(Error("divergent"))
      else
        innerComplete[R](seen + this)
    }

    protected def innerComplete[R](seen: Set[Parser[Token, _]]): Error[Token, R] \/ Completed[Token, Result]

    // progress the parse over one token
    final def derive(t: Token): State[Cache[Token], Parser[Token, Result]] = for {
      cache <- State.get[Cache[Token]]
      back <- cache get (t -> this) map { State.state[Cache[Token], Parser[Token, Result]](_) } getOrElse innerDerive(t)

      cache2 = cache + ((t, this) -> back)
      _ <- State.put(cache2)
    } yield back

    protected def innerDerive(candidate: Token): State[Cache[Token], Parser[Token, Result]]
  }
}

private[parsers] class SeqParser[Token, LR, RR](_left: => Parser[Token, LR], _right: => Parser[Token, RR]) extends Parser.Incomplete[Token, LR ~ RR] {
  import Parser._

  private lazy val left = _left
  private lazy val right = _right

  def innerComplete[R](seen: Set[Parser[Token, _]]): Error[Token, R] \/ Completed[Token, LR ~ RR] = for {
    Completed(lr) <- left.complete[R](seen)
    Completed(rr) <- right.complete[R](seen)
  } yield Completed((lr, rr))

  def innerDerive(t: Token): State[Cache[Token], Parser[Token, LR ~ RR]] = (left, right) match {
    case (Completed(_), Completed(_)) | (Completed(_), Error(_)) => State.state(Error("unexpected end of stream"))

    case (Error(msg), _) => State.state(Error(msg))
    case (_, Error(msg)) => State.state(Error(msg))

    case (Completed(lr), right: Incomplete[Token, RR]) => for {
      rp <- right derive t
    } yield rp map { (lr, _) }

    case (left: Incomplete[Token, LR], Completed(rr)) => for {
      lp <- left derive t
    } yield lp map { (_, rr) }

    case (left: Incomplete[Token, LR], right: Incomplete[Token, RR]) => {
      left.complete(Set()).toOption map {
        case Completed(lr) => {
          for {
            lp <- left derive t       // TODO insufficiently lazy!  when this is an error, the LEFT side should be an error, not the whole thing
            rp <- right derive t
          } yield lp ~ right | (rp map { (lr, _) })
        }
      } getOrElse {
        for {
          lp <- left derive t
        } yield lp ~ right
      }
    }
  }
}

private[parsers] class UnionParser[Token, Result](_left: => Parser[Token, Result], _right: => Parser[Token, Result]) extends Parser.Incomplete[Token, Result] {
  import Parser._

  private lazy val left = _left
  private lazy val right = _right

  def innerComplete[R](seen: Set[Parser[Token, _]]): Error[Token, R] \/ Completed[Token, Result] = {
    (left complete seen, right complete seen) match {
      case (\/-(Completed(_)), \/-(Completed(_))) => -\/(Error("global ambiguity detected"))
      case (lr @ \/-(Completed(_)), -\/(Error(_))) => lr
      case (-\/(Error(_)), rr @ \/-(Completed(_))) => rr
      case (-\/(Error(msg)), -\/(Error(msg2))) => {
        if (msg == msg2)
          -\/(Error(msg))
        else
          -\/(Error(s"$msg and $msg2"))
      }
    }
  }

  def innerDerive(t: Token): State[Cache[Token], Parser[Token, Result]] = (left, right) match {
    case (Error(leftMsg), Error(rightMsg)) => State.state(Error(s"$leftMsg OR $rightMsg"))

    case (Error(_), Completed(_)) => State.state(Error("unexpected end of stream"))
    case (Completed(_), Error(_)) => State.state(Error("unexpected end of stream"))
    case (Completed(_), Completed(_)) => State.state(Error("unexpected end of stream"))

    case (Error(_) | Completed(_), right: Incomplete[Token, Result]) => right derive t
    case (left: Incomplete[Token, Result], Error(_) | Completed(_)) => left derive t

    case (left: Incomplete[Token, Result], right: Incomplete[Token, Result]) => for {
      lp <- left derive t
      rp <- right derive t
    } yield lp | rp         // TODO this value needs to be in the Cache[Token] *before* we recurse
  }
}
