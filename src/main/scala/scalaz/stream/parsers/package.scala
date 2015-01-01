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

import scalaz._
import scalaz.syntax.show._

import scala.util.matching.Regex

package object parsers {
  import Process._

  type ~[+A, +B] = (A, B)

  object ~ {
    def unapply[A, B](in: (A, B)): Some[(A, B)] = Some(in)
  }

  /**
   * Greedily parses and emits as rapidly as possible.  Note that this may not be
   * exactly what you want!  The exact semantics here are to use the parser to
   * consume tokens as long as the parser *requires* more tokens in order to emit
   * a valid output, or until the parser derives an error on all branches.  Thus,
   * as soon as the parser can possibly emit a valid value, it will do so.  At
   * that point, the parser state is flushed and restarted with the next token.
   * This is sort of the moral equivalent of the `findPrefixOf` function on
   * Regex, but applied repeatedly to the input stream.
   *
   * I can't quite contrive a scenario where these semantics result in undesirable
   * behavior, but I'm sure they exist.  If nothing else, it seems clear that there
   * are a lot of arbitrary defaults baked into these semantics, and it would be
   * nice to have a bit more control.
   *
   * Sidebar: this function *does* attempt to prevent completely vacuuous parse
   * results.  Providing a parser which accepts the empty string will not result
   * in an infinite stream of successes without consuming anything.
   */
  def parse[Token: Show, Result](parser: Parser[Token, Result]): Process1[Token, String \/ Result] = {
    import Parser._

    def inner(parser: Parser[Token, Result]): State[Cache[Token], Process1[Token, String \/ Result]] = State.get[Cache[Token]] map { cache =>
      receive1Or[Token, String \/ Result](emit(parser.complete[Result]().bimap({ _.msg }, { _.result }))) { token =>
        parser match {
          case Completed(r) => emit(-\/(s"input parser does not expect further input; got ${token.shows}"))
          case Error(str) => emit(-\/(str))

          case parser: Incomplete[Token, Result] => {
            val innerState: State[Cache[Token], Process1[Token, String \/ Result]] = for {
              derived <- parser derive token

              back <- derived.complete() match {
                case -\/(Error(_)) => inner(derived)
                case \/-(Completed(r)) => State.state[Cache[Token], Process1[Token, String \/ Result]](emit(\/-(r)))
              }
            } yield back

            /*
             * This is hugely tricky.  We're throwing away our derived state here.  This is FINE
             * though, because we're not going to preserve the state into the next parse trail!
             * Basically, this line is taking advantage of the fact that we begin each parse with
             * an empty cache and we *don't* delegate to `inner` multiple times in the above
             * for-comprehension.  If we were to repeatedly delegate to `inner`, the following line
             * would be unsound.
             */
            innerState eval cache
          }
        }
      }
    }

    // parse as much as we can, restarting with each completed parse
    (inner(parser) eval Cache[Token]) ++ parse(parser)
  }

  /**
   * Somewhat-inefficiently (but usefully!) tokenizes an input stream of characers into
   * a stream of tokens given a set of regular expressions and mapping functions.  Note
   * that the resulting process will have memory usage which is linearly proportional to
   * the longest *invalid* substring, so…be careful.  There are better ways to implement
   * this function.  MUCH better ways.
   */
  def tokenize[T](rules: Map[Regex, PartialFunction[List[String], T]], whitespace: Option[Regex] = Some("""\s+""".r)): Process1[Char, Char \/ T] = {
    import Process._

    def iseqAsCharSeq(seq: IndexedSeq[Char]): CharSequence = new CharSequence {
      def charAt(i: Int) = seq(i)
      def length = seq.length
      def subSequence(start: Int, end: Int) = iseqAsCharSeq(seq.slice(start, end))
      override def toString = seq.mkString
    }

    def attempt(buffer: CharSequence, requireIncomplete: Boolean): Option[T] = {
      def matchBoth(pattern: Regex, pf: PartialFunction[List[String], T]): Boolean =
        pattern findFirstMatchIn buffer filter { !requireIncomplete || _.matched.length < buffer.length } map { _.subgroups } collect pf isDefined

      rules collectFirst {
        case (pattern, pf) if matchBoth(pattern, pf) =>
          pattern findFirstMatchIn buffer map { _.subgroups } collect pf get      // I hate how we have to split this...
      }
    }

    /*
     * Buffer up characters until we get a prefix match PLUS one character that doesn't match (this
     * is to defeat early-completion of greedy matchers).  Once we get a prefix match that satisfies
     * a rule in the map, emit the resulting token and flush the buffer.  Any characters that aren't
     * matched by any rule are emitted.
     *
     * Note that the `tokenize` function should probably have a maxBuffer: Int parameter, or similar,
     * since it would be possible to DoS this function by simply feeding an extremely long unmatched
     * prefix.  Better yet, we should just knuckle-down and write a DFA compiler.  Would be a lot
     * simpler.
     */
    def inner(buffer: Vector[Char]): Process1[Char, Char \/ T] = {
      receive1Or[Char, Char \/ T](attempt(iseqAsCharSeq(buffer), false) map { \/-(_) } map emit getOrElse emitAll(buffer map { -\/(_) })) { c =>
        val buffer2 = buffer :+ c
        val csBuffer = iseqAsCharSeq(buffer2)

        val wsMatch = whitespace flatMap { _ findPrefixOf csBuffer }

        // if we matched prefix whitespace, move on with a clean buffer
        wsMatch map { prefix =>
          inner(buffer2 drop prefix.length)
        } getOrElse {
          attempt(csBuffer, true) map { \/-(_) } map { t => emit(t) ++ inner(Vector(c)) } getOrElse inner(buffer2)
        }
      }
    }

    inner(Vector.empty)
  }
}
