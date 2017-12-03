package io.hnfmr.chapter11

import cats.Monoid
import cats.instances.list._
import cats.instances.map._
import cats.syntax.semigroup._
import cats.syntax.foldable._

final case class GCounter[A](counters: Map[String, A]) {
  def increment(machine: String, amount: A)(implicit m: Monoid[A]): GCounter[A] =
    GCounter(counters + (machine -> (amount |+| counters.getOrElse(machine, Monoid.empty[A]))))

  def total(implicit m: Monoid[A]): A = this.counters.values.toList.combineAll

  def merge(that: GCounter[A])(implicit b: BoundedSemiLattice[A]): GCounter[A] =
    GCounter(this.counters |+| that.counters)
}