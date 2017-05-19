package io.hnfmr.chapter1

import cats.Eq
import cats.syntax.eq._
import cats.syntax.option._
import cats.instances.int._
import cats.instances.option._
import cats.instances.long._
import cats.instances.string._

import java.util.Date

object EqTut extends App {
  val a = (123 === 123)
  val b = (Some(10): Option[Int]) === (None: Option[Int])

  val c = Option(1) === Option.empty[Int]

  val d = 1.some === None

  val e = 1.some =!= None

  implicit val dateEqual = Eq.instance[Date] { (date1, date2) =>
    date1.getTime === date2.getTime
  }

  val x = new Date()
  val y = new Date()

  val j = x === x
  val k = x === y

  final case class Cat ( name: String, age: Int, color: String )
  implicit val catEqual = Eq.instance[Cat] { (cat1, cat2) =>
    (cat1.age === cat2.age) &&
      (cat1.color === cat2.color) &&
      (cat1.name === cat2.name)
  }

  val cat1 = Cat("Garfield", 35, "orange and black")
  val cat2 = Cat("Heathcliff", 30, "orange and black")

  val optCat1 = Option(cat1)
  val optCat2 = Option.empty[Cat]

  val cc = optCat1 === optCat2
}
