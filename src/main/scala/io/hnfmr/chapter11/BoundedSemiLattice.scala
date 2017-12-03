package io.hnfmr.chapter11

import cats.Monoid

trait BoundedSemiLattice[A] extends Monoid[A] {
  def combine(a1: A, a2: A): A
  def empty: A
}

object BoundedSemiLattice {
  implicit val intBsl: BoundedSemiLattice[Int] =
    new BoundedSemiLattice[Int] {
      override def empty: Int = 0

      override def combine(a1: Int, a2: Int): Int = a1 max a2
    }

  implicit def setBsl[A]: BoundedSemiLattice[Set[A]] =
    new BoundedSemiLattice[Set[A]] {
      override def empty: Set[A] = Set.empty[A]

      override def combine(a1: Set[A], a2: Set[A]): Set[A] =
        a1 union a2
    }
}