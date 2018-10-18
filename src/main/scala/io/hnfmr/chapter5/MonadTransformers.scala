package io.hnfmr.chapter5

import cats.instances.either._
import cats.syntax.applicative._
import cats.data.OptionT

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import cats.data.EitherT
import cats.instances.future._

object MonadTransformers extends App {
  type Error = String

  type ErrorOr[A] = Either[Error, A]

  type ErrorOptionOr[A] = OptionT[ErrorOr, A]

  val result1: ErrorOptionOr[Int] = 41.pure[ErrorOptionOr]

  val result2: ErrorOptionOr[Int] = result1.flatMap(x => (x + 1).pure[ErrorOptionOr])

  println(result1)
  println(result2)

  type FutureEither[A] = EitherT[Future, String, A]
  type FutureEitherOption[A] = OptionT[FutureEither, A]

  val answer: FutureEitherOption[Int] =
    for {
      a <- 10.pure[FutureEitherOption]
      b <- 0.pure[FutureEitherOption]
    } yield a + b

  Thread.sleep(200)
  println(answer.value)
}
