package io.hnfmr.chapter4

import scala.language.higherKinds
import cats.Monad
import cats.Id

import cats.syntax.flatMap._
import cats.syntax.functor._

import cats.syntax.applicative._
import cats.syntax.option._
import cats.instances.option._
import cats.instances.list._

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import cats.instances.future._

object MonadTut extends App {
  object Tutorial {
    val a = 1.pure[Option]
    val b = 1.pure[List]

    println(b)
  }

  def sumSquare[M[_] : Monad](a: M[Int], b: M[Int]): M[Int] =
    for {
      x <- a
      y <- b
    } yield x*x + y*y

  println(sumSquare(3.some, 4.some))
  println(sumSquare(1.some, Option.empty[Int]))
  println(sumSquare(List(1,2,3), List(4,5)))

  val e: Id[Int] = Monad[Id].pure(3)
  val f: Id[Int] = 4
  println(sumSquare(e, f))
  println(Await.result(sumSquare(Future(3), Future(4)), 1.second))
}
