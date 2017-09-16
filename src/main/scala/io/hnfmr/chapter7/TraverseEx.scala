package io.hnfmr.chapter7

import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import scala.language.higherKinds
import cats.Applicative
import cats.syntax.applicative._
import cats.Apply
import cats.instances.future._
import cats.instances.vector._
import cats.instances.option._
import cats.data.Validated
import cats.instances.list._

object TraverseEx extends App {
  val hostnames = List(
    "www.google.com",
    "www.facebook.com"
  )

  def getUptime(hostname: String): Future[Int] =
    Future(hostname.length * 60)

  val allUptimes: Future[List[Int]] = Future.traverse(hostnames)(getUptime)

  println(
    Await.result(allUptimes, 1.second)
  )

  // traverse list of applicatives
  def listTraverse[F[_] : Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      Apply[F].map2(accum, func(item))(_ :+ _)
    }

  def listSequence[F[_] : Applicative, B](list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

  println(
    Await.result(listTraverse(hostnames)(getUptime), 1.second)
  )

  println(
    listSequence(List(Vector(1,2), Vector(3,4)))
  )

  println(
    listSequence(List(Vector(1,2), Vector(3,4), Vector(5,6)))
  )

  println(
    process(List(2,4,6))
  )

  def process(inputs: List[Int]) =
    listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)

  println(
    process(List(1,2,3))
  )

  type ErrorsOr[A] = Validated[List[String], A]
  def process2(inputs: List[Int]): ErrorsOr[List[Int]] =
    listTraverse(inputs) { n =>
      if (n % 2 == 0) {
        Validated.valid(n)
      } else {
        Validated.invalid(List(s"$n is not even"))
      }
    }

  println(
    process2(List(2,4,6))
  )

  println(
    process2(List(1,2,3))
  )
}
