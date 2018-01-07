package io.hnfmr.chapter11

import cats.Monoid
import scala.language.higherKinds

trait CRDTCounter[F[_,_], K, V] {
  def increment(f: F[K, V])(k: K, v: V)(implicit m: Monoid[V]): F[K, V]

  def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]

  def total(f: F[K, V])(implicit m: Monoid[V]): V
}

object CRDTCounter {
  def apply[F[_,_], K, V](implicit counter: CRDTCounter[F, K, V]) = counter
}

