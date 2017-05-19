package io.hnfmr.chapter1

import cats.Show
import cats.instances.int._
import cats.instances.string._

import cats.syntax.show._

object ShowTut extends App {
  val showInt: Show[Int] = Show.apply[Int]
  val showString: Show[String] = Show.apply[String]

  val intAsString: String = showInt.show(123)
  val stringAsString: String = showString.show("abc")

  val shownInt = 123.show
  val shownString = "abc".show
}
