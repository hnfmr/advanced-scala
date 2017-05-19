package io.hnfmr.chapter1

import cats.Eq
import cats.syntax.eq._
import cats.syntax.option._
import cats.instances.int._
import cats.instances.option._

object EqTut extends App {
  val a = (123 === 123)
  val b = (Some(10): Option[Int]) === (None: Option[Int])

  val c = Option(1) === Option.empty[Int]

  val d = 1.some === None

  val e = 1.some =!= None
}
