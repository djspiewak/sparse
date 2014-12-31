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
