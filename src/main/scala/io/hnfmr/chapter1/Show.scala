package io.hnfmr.chapter1

import cats.Show
import cats.instances.int._
import cats.instances.string._

object Show extends App {
  val showInt: Show[Int] = Show.apply[Int]
  val showString: Show[String] = Show.apply[String]
}
