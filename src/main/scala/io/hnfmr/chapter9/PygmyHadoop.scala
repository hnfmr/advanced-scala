package io.hnfmr.chapter9

import cats.Monoid
import cats.Monad

import cats.syntax.semigroup._
import cats.instances.string._

import cats.instances.future._
import cats.instances.int._
import cats.instances.list._
import cats.instances.vector._
import cats.syntax.traverse._
import cats.syntax.foldable._

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object PygmyHadoop extends App {

  // my solution
  def foldMap[A, B : Monoid](seq: Vector[A])(f: A => B): B =
    seq.map( v => f(v) ).fold(Monoid[B].empty)( (a, b) => Monoid[B].combine(a, b))

  // alt solution 1:
  def foldMap1[A, B : Monoid](seq: Vector[A])(f: A => B): B =
    seq.map(f).fold(Monoid[B].empty)( _ |+| _)

  // alt solution 2:
  def foldMap2[A, B : Monoid](seq: Vector[A])(f: A => B): B =
    seq.foldLeft(Monoid[B].empty)(_ |+| f(_))

  println(
    foldMap(Vector(1,2,3))(_.toString + "! ")
  )

  println(
    foldMap(Vector(1,2,3))(_ * 3)
  )

  println(
    foldMap(Vector(1,2,3))(identity)
  )

  println(
    foldMap("Hello World!".toVector)(_.toString.toUpperCase)
  )

  println(Monad[Future].pure(42))
  println(
    Await.result
    (
      Monoid[Future[Int]].combine(Future(1), Future(2)),
      1.second
    )
  )

  // 9.3.3 parallelFoldMap

  def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val numOfCores = Runtime.getRuntime.availableProcessors()
    val fs: List[Future[B]] = values.grouped( values.length / numOfCores).map(
      vs => Future(foldMap2(vs)(func))
    ).toList

    fs.foldLeft(Monoid[Future[B]].empty)( (a, e) => Monoid[Future[B]].combine(a, e) )
  }

  println(
    Await.result(
      parallelFoldMap((1 to 100).toVector)(_ + 1),
      1.second
    )
  )

  // cats version
  def parallelFoldMap1[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val numOfCores = Runtime.getRuntime.availableProcessors()
    values
      .grouped(values.length / numOfCores)
      .toList
      .traverse(vs => Future(vs.foldMap(func)))
      .map(_.combineAll)
  }

  println(
    Await.result(
      parallelFoldMap1((1 to 100).toVector)(_ + 1),
      1.second
    )
  )
}