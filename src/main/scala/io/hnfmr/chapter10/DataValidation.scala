package io.hnfmr.chapter10

import cats.Semigroup
import cats.syntax.either._
import cats.syntax.semigroup._
import cats.instances.list._

object DataValidation extends App {

  final case class CheckF[E, A](f: A => Either[E, A]) {
    def apply(a: A): Either[E, A] = f(a)

    def and(that: CheckF[E, A])(implicit s: Semigroup[E]): CheckF[E, A] =
      CheckF { a =>
        (this (a), that(a)) match {
          case (Left(l), Left(r)) => Left(Semigroup[E].combine(l, r))
          case (Left(l), Right(r)) => Left(l)
          case (Right(l), Right(r)) => Right(r)
          case (Right(l), Left(r)) => Left(r)
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
  }


  val check1 = CheckF {
    a: Int => if (a < 10) Left(List(s"$a is too small, must be >= 10")) else Right(a)
  }

  val check2 = CheckF {
    a: Int => if (a % 2 != 0) Left(List(s"$a is odd, must be even")) else Right(a)
  }

  val check3 = check1 and check2

  println(check3(7))
  println(check3(8))
  println(check3(12))
  println(check3(13))
}
