package scalaz.stream

package object parsers {

  type ~[+A, +B] = (A, B)

  object ~ {
    def unapply[A, B](in: (A, B)): Some[(A, B)] = Some(in)
  }
}
