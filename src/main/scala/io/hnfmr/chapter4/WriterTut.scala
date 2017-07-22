package io.hnfmr.chapter4

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

import cats.data.Writer
import cats.instances.vector._
import cats.syntax.writer._
import cats.syntax.applicative._

object WriterTut extends App {

  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  type Logged[A] = Writer[Vector[String], A]
  def factorial(n: Int): Logged[Int] =
    for {
      ans <- slowly ( if (n == 0) 1.pure[Logged] else factorial( n - 1 ).map(_ * n) )
      _ <- Vector(s"fact $n $ans").tell
    } yield ans


  val Vector( (la, a), (lb, b) ) =
    Await.result(Future.sequence(Vector(
      Future(factorial(3).run),
      Future(factorial(3).run)
    )), 5.seconds)

  println(s"($la, $a), ($lb, $b)")
}
