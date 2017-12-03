package io.hnfmr.chapter6

import cats.Semigroupal
import cats.instances.future._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import cats.syntax.apply._
import cats.instances.list._

object SemigroupalTut extends App {
  val futurePair = Semigroupal[Future].
    product(Future("Hello"), Future(123))

  val res = Await.result(futurePair, 1.second)
  println(res)

  case class Cat( name: String, yearOfBirth: Int, favoriteFoods: List[String] )

  val futureCat = (
    Future("Garfield"),
    Future(1978),
    Future(List("Sausage"))
  ).mapN(Cat.apply)

  val res0 = Await.result(futureCat, 1.second)
  println(res0)
}
