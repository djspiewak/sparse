package scalaz.stream
package parsers

import scalaz._
import scalaz.syntax.equal._
import scalaz.syntax.monad._
import scalaz.syntax.show._

/**
 * Applicative (not monadic!) parser interface defined by two functions (simplified types):
 *
 * - `complete: Error \/ Completed`
 * - `derive: State[Cache, Parser]`
 *
 * The `derive` function is only defined on parsers of the subtype, `Incomplete`.  The `complete`
 * function is defined on all parsers, where the following axioms hold:
 *
 * - `complete Completed = \/-(Completed)`
 * - `complete Error = -\/(Error)`
 * - `complete Incomplete = ???`
 *
 * Which is to say that an "Incomplete" parser may be completable, but is also guaranteed to have
 * potential subsequent derivations.  A "Complete" or "Error" parser do not have any further
 * derivations, but their completeness is guaranteed.  An example of an incomplete parser that has
 * subsequent possible derivations but is still completeable is the following:
 *
 * lazy val parens = (
 *     '(' ~ parens ~ ')'
 *   | completed
 * )
 *
 * The `parens` parser may be completed immediately, since it contains a production for the empty
 * string.  However, it may also be derived, and the only valid derivation for it is over the '('
 * token.  The resulting parser from that derivation *cannot* be completed, since it would require
 * a matching paren in order to represent a valid input.
 *
 * A parser which starts as Incomplete and then becomes either Completed or Error might be something
 * like the following:
 *
 * lazy val foo = literal('a')
 *
 * The `foo` parser is Incomplete and not completable (it contains no production for the empty string).
 * However, it may be derived over the token 'a' to produce a Completed parser (which will actually
 * be of runtime type Completed).  If it is derived over any other token, it will produce an Error
 * parser.
 *
 * Thus, unlike many parser combinators encodings, this one encodes the result algebra directly in
 * the parser itself.  This has several advantages from a usability standpoint.  It does, however,
 * make the encoding somewhat convoluted in a few places from an implementation standpoint.  Hopefully
 * those convolutions do not leak into user space...
 */
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
  type Cache[Token] = KMap[({ type λ[α] = (Token, Parser[Token, α]) })#λ, ({ type λ[α] = () => Parser[Token, α] })#λ]

  /**
   * Parser for the empty string, producing a given result.
   */
  def completed[Token, Result](r: Result): Parser[Token, Result] = Completed(r)

  /**
   * Parser that is already in the error state.  Generally speaking, this is probably
   * only useful for internal plumbing.
   */
  def error[Token, Result](msg: String): Parser[Token, Result] = Error(msg)

  /**
   * Parser for a single literal token, producing that token as a result.  Parametricity!
   */
  implicit def literal[Token: Equal: Show](token: Token): Parser[Token, Token] = new Incomplete[Token, Token] {

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
  // syntax
  //

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

  implicit class Caret1[Token, Result](self: Parser[Token, Result]) {

    /**
     * Alias for map
     */
    def ^^[Result2](f: Result => Result2): Parser[Token, Result2] = self map f
  }

  implicit class Caret2[Token, A, B](self: Parser[Token, A ~ B]) {

    def ^^[Z](f: (A, B) => Z): Parser[Token, Z] = self map {
      case a ~ b => f(a, b)
    }
  }

  implicit class Caret3L[Token, A, B, C](self: Parser[Token, (A ~ B) ~ C]) {

    def ^^[Z](f: (A, B, C) => Z): Parser[Token, Z] = self map {
      case (a ~ b) ~ c => f(a, b, c)
    }
  }

  implicit class Caret3R[Token, A, B, C](self: Parser[Token, A ~ (B ~ C)]) {

    def ^^[Z](f: (A, B, C) => Z): Parser[Token, Z] = self map {
      case a ~ (b ~ c) => f(a, b, c)
    }
  }

  implicit class Caret4LL[Token, A, B, C, D](self: Parser[Token, ((A ~ B) ~ C) ~ D]) {

    def ^^[Z](f: (A, B, C, D) => Z): Parser[Token, Z] = self map {
      case ((a ~ b) ~ c) ~ d => f(a, b, c, d)
    }
  }

  implicit class Caret4LR[Token, A, B, C, D](self: Parser[Token, (A ~ (B ~ C)) ~ D]) {

    def ^^[Z](f: (A, B, C, D) => Z): Parser[Token, Z] = self map {
      case (a ~ (b ~ c)) ~ d => f(a, b, c, d)
    }
  }

  implicit class Caret4RL[Token, A, B, C, D](self: Parser[Token, A ~ ((B ~ C) ~ D)]) {

    def ^^[Z](f: (A, B, C, D) => Z): Parser[Token, Z] = self map {
      case a ~ ((b ~ c) ~ d) => f(a, b, c, d)
    }
  }

  implicit class Caret4RR[Token, A, B, C, D](self: Parser[Token, A ~ (B ~ (C ~ D))]) {

    def ^^[Z](f: (A, B, C, D) => Z): Parser[Token, Z] = self map {
      case a ~ (b ~ (c ~ d)) => f(a, b, c, d)
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

    /**
     * Progresses the parse over a single token and returns the continuation (as a parser).  Note that
     * the cache carried in the state monad is very important and must be preserved for the duration
     * of an uncompletable parse.  Once a parser resulting from this derivation is completable, that
     * completion may be invoked and the state dropped.  Dropping state in the middle of an incomplete
     * parse will yield unsound results and possibly divergent parse trails!
     *
     * As the parametricity implies, this derivation function does not advance the parse over anything
     * more than a single token, even if that single token taken in context with the state of the
     * parse coming in cannot yield a valid output.  For example, imagine a parser for matching
     * parentheses.  One could advance the parser over a token representing '('.  This could not
     * possibly yield a completable parser, since it is impossible for a correctly formed parentheses
     * grammar to find a match for a newly-opened parenthetical.  However, the derivation function
     * will still return immediately after consuming the '(' token.  The resulting parser can be used
     * to advance over subsequent tokens, but cannot be completed then-and-there (attempting to do
     * so would result in an Error).
     */
    final def derive(t: Token): State[Cache[Token], Parser[Token, Result]] = State { cache =>
      // derived needs to be in the cache which is used in its derivation, otherwise direct recursion diverges
      lazy val (cache3: Cache[Token], derived: Parser[Token, Result]) = innerDerive(t).run(cache2)
      lazy val cache2: Cache[Token] = cache + ((t, this) -> { () => derived })

      // if we've already derived, we have the contents of cache3 -- cache2 inside of cache
      // if we haven't derived, then cache3 contains all the contents of cache2 -- cache
      cache get (t -> this) map { thunk => (cache, thunk()) } getOrElse (cache3, derived)
    }

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
            lp <- left derive t
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
    } yield lp | rp
  }
}
