package io.hnfmr.chapter6

import cats.Semigroupal
import cats.data.Validated
import cats.instances.list._
import cats.syntax.validated._

object ValidatedTut extends App {
  type AllErrorsOr[A] = Validated[List[String], A]

  val r = Semigroupal[AllErrorsOr].product(
    Validated.invalid(List("Error 1")),
    Validated.invalid(List("Error 2"))
  )

  println(r)

  println("xx".invalid[Int])
  println("xx".invalid[Int].toEither)
  println(123.valid)
}
