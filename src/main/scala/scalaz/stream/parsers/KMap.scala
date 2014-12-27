package scalaz.stream
package parsers

private[parsers] class KMap[K[_] <: AnyRef, V[_] <: AnyRef] private (delegate: Map[AnyRef, AnyRef]) {

  def apply[A](key: K[A]): V[A] = delegate(key).asInstanceOf[V[A]]

  def get[A](key: K[A]): Option[V[A]] = delegate.get(key).asInstanceOf[Option[V[A]]]

  def +[A](pair: (K[A], V[A])): KMap[K, V] =
    new KMap[K, V](delegate + pair.asInstanceOf[(AnyRef, AnyRef)])

  def contains[A](key: K[A]): Boolean = delegate contains key
}

private[parsers] object KMap {

  def apply[K[_] <: AnyRef, V[_] <: AnyRef](pairs: ((K[A], V[A]) forSome { type A })*): KMap[K, V] =
    new KMap[K, V](Map(pairs map { _.asInstanceOf[(AnyRef, AnyRef)] }: _*))
}
