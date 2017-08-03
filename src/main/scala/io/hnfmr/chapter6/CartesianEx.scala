package io.hnfmr.chapter6

import scala.language.higherKinds
import cats.Monad

import cats.instances.option._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._

object CartesianEx extends App {
  def product[M[_]: Monad, A, B] ( fa: M[A] ,fb: M[B] ): M[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)

  println(product(3.some, 4.some))
  println(product(3.some, none))
}
