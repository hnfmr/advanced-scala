package io.hnfmr.chapter10

import cats.instances.list._

import cats.Semigroupal
import cats.data.NonEmptyList
import cats.data.Kleisli
import cats.syntax.either._
import cats.instances.either._

import Functions._

object KleisliEx extends App {
  val step1: Kleisli[List, Int, Int] =
    Kleisli( (x:Int) => List(x + 1, x, x - 1))

  val step2: Kleisli[List, Int, Int] =
    Kleisli( x => List(x, -x))

  val step3: Kleisli[List, Int, Int] =
    Kleisli(x => List( x * 2, x / 2))

  val pipeline = step1 andThen step2 andThen step3

  println(pipeline.run(20))


  type Result[A] = Either[Errors, A]
  type CheckK[A, B] = Kleisli[Result, A, B]

  def check[A, B](f: A => Either[Errors, B]): CheckK[A, B] =
    Kleisli(f)

  def checkPred[A](pred:Predicate[Errors, A]): CheckK[A, A] =
    Kleisli[Result, A, A](pred.run)

  val userNameCheck: Check[Errors, String, String] =
    Check(
      longerThan(4) and alphanumeric
    )

  val checkLeft: CheckK[String, String] = checkPred(longerThan(0))
  val checkRight: CheckK[String, String] = checkPred(longerThan(3) and containsOnce('.'))

  def splitCheck: CheckK[String, (String, String)] =
    check { s =>
      s.split("@") match {
        case Array(first, second) => (first, second).asRight
        case _ => NonEmptyList("Must contain a single @ character", Nil).asLeft
      }
    }

  def joinEmail: CheckK[(String, String), String] =
    check { case (f, s) =>
      Semigroupal.map2 (checkLeft (f), checkRight (s) ) (_+ "@" + _)
    }

  val emailCheck: CheckK[String, String] =
    splitCheck andThen joinEmail

  println("-"*50)
  println(emailCheck("what@at.com"))
  println(emailCheck("what@at..com"))
  println(emailCheck("@at..com"))
}
