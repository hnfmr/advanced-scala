package io.hnfmr.chapter10

import cats.Semigroup
import cats.syntax.either._
import cats.syntax.semigroup._
import cats.instances.list._

import cats.data.Validated
import cats.data.Validated.{Valid, Invalid}
import cats.syntax.apply._

object DataValidation extends App {

  // CheckF - based on combinators
  final case class CheckF[E, A](f: A => Either[E, A]) {
    def apply(a: A): Either[E, A] = f(a)

    def and(that: CheckF[E, A])(implicit s: Semigroup[E]): CheckF[E, A] =
      CheckF { a =>
        (this (a), that(a)) match {
          case (Left(l), Left(r)) => Left(Semigroup[E].combine(l, r))
          case (Left(l), Right(_)) => Left(l)
          case (Right(_), Right(r)) => Right(r)
          case (Right(_), Left(r)) => Left(r)
        }
      }

    def and1(that: CheckF[E, A])(implicit s: Semigroup[E]): CheckF[E, A] =
      CheckF { a =>
        (this (a), that(a)) match {
          case (Left(l), Left(r)) => (l |+| r).asLeft
          case (Left(l), Right(r)) => l.asLeft
          case (Right(l), Right(r)) => r.asRight
          case (Right(l), Left(r)) => r.asLeft
        }
      }

    def or(that: CheckF[E, A])(implicit s: Semigroup[E]): CheckF[E, A] =
      CheckF { a =>
        (this (a), that(a)) match {
          case (Left(l), Left(r)) => (l |+| r).asLeft
          case (Left(_), Right(r)) => r.asRight
          case (Right(_), Right(r)) => r.asRight
          case (Right(l), Left(r)) => l.asRight
        }
      }
  }


  val check1 = CheckF {
    a: Int => if (a < 10) Left(List(s"$a is too small, must be >= 10")) else Right(a)
  }

  val check2 = CheckF {
    a: Int => if (a % 2 != 0) Left(List(s"$a is odd, must be even")) else Right(a)
  }

  val check3 = check1 and1 check2

  println(check3(7))
  println(check3(8))
  println(check3(12))
  println(check3(13))

  println("-"*50)
  val check4 = check1 or check2

  println(check4(7))
  println(check4(8))
  println(check4(12))
  println(check4(13))

  // Check - based on ADT
  sealed trait Check[E, A] {
    def and(that: Check[E, A]): Check[E, A] =
      And(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      this match {
        case Pure(f) => f(a)
        case And(l, r) => (l(a), r(a)).mapN( (_,_) => a)
        case Or(l, r) => (l(a), r(a)) match {
          case (Valid(_), Valid(y)) => Valid(y)
          case (Valid(x), Invalid(_)) => Valid(x)
          case (Invalid(x), Invalid(y)) => Invalid(x |+| y)
          case (Invalid(_), Valid(y)) => Valid(y)
        }
      }
  }

  final case class And[E, A]( left: Check[E, A], right: Check[E, A] ) extends Check[E, A]
  final case class Pure[E, A](f: A => Validated[E, A]) extends Check[E, A]
  final case class Or[E, A]( left: Check[E, A], right: Check[E, A]) extends Check[E, A]

  val check5 = Pure {
    a: Int => if (a < 10) Validated.Invalid(List(s"$a is too small, must be >= 10")) else
      Validated.Valid(a)
  }

  val check6 = Pure {
    a: Int => if (a % 2 != 0) Validated.Invalid(List(s"$a is odd, must be even")) else Validated.Valid(a)
  }

  val check7 = And(check5, check6)
  val check8 = Or(check5, check6)

  println("-"*50)

  println(check7(7))
  println(check7(8))
  println(check7(12))
  println(check7(13))

  println("-"*50)

  println(check8(7))
  println(check8(8))
  println(check8(12))
  println(check8(13))
}
