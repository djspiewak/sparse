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
}
