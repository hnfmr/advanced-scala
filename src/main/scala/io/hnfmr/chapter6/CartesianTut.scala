package io.hnfmr.chapter6

import cats.Monoid
import cats.instances.boolean._
import cats.instances.int._
import cats.instances.list._
import cats.instances.string._
import cats.syntax.cartesian._
import cats.syntax.semigroup._
import cats.instances.monoid._

object CartesianTut extends App {

  case class Cat(name: String,
                 yearOfBirth: Int,
                 favoriteFoods: List[String]
                )

  def catToTuple(cat: Cat): (String, Int, List[String]) =
    (cat.name, cat.yearOfBirth, cat.favoriteFoods)

  implicit val catMonoid: Monoid[Cat] = (
    Monoid[String] |@|
    Monoid[Int] |@|
    Monoid[List[String]]
  ).imap(Cat.apply)(catToTuple)

  println(Monoid[Cat].empty)

  val garfield = Cat("Garfield", 1978, List("Lasagne"))
  val heathcliff = Cat("Heathcliff", 1988, List("Junk Food"))

  println(garfield |+| heathcliff)
}