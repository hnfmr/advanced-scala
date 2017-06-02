package io.hnfmr.chapter4

import cats.syntax.either._

object EitherTut extends App {
  // Either in Scala 2.12 is right-biased
  val either0 = Right(123)
  val either1 = either0.flatMap(x => Right(x + 1))
  val either2 = either0.flatMap(x => Right(x + 2))
  val either3 = "DIV0".asLeft[Int]

  val d = for {
    a <- either1
    b <- either2
    c <- either3 // fail-fast error handling
  } yield a + b + c

  println(d)
}
