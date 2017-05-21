package io.hnfmr.chapter2

import cats.Monoid
import cats.Semigroup
import cats.syntax.semigroup._
import cats.instances.string._
import cats.instances.int._

object CatMonoidSemigroupTut extends App {
  val a = Monoid[String].combine("Hi", "There")

  val b = Monoid[String].empty

  Monoid.apply[String].empty
  Monoid.apply[String].combine("Hi ", "There")

  val d = Semigroup.apply[String].combine("Hi ", "There")

  val stringResult = "Hi " |+| "there" |+| Monoid[String].empty

  val intResult = 1 |+| 2 |+| Monoid[Int].empty
}
