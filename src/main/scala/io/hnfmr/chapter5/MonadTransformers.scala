package io.hnfmr.chapter5

import cats.instances.either._
import cats.syntax.applicative._
import cats.data.OptionT

object MonadTransformers extends App {
  type Error = String

  type ErrorOr[A] = Either[Error, A]

  type ErrorOptionOr[A] = OptionT[ErrorOr, A]

  val result1: ErrorOptionOr[Int] = 41.pure[ErrorOptionOr]

  val result2: ErrorOptionOr[Int] = result1.flatMap(x => (x + 1).pure[ErrorOptionOr])

  println(result1)
  println(result2)
}


