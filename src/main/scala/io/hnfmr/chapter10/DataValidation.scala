package io.hnfmr.chapter10

import cats.Semigroup
import cats.syntax.either._
import cats.syntax.semigroup._

object DataValidation extends App {

  final case class CheckF[E, A](f: A => Either[E, A]) {
    def apply(a: A): Either[E, A] = f(a)

    def and(that: CheckF[E, A])(implicit s: Semigroup[E]): CheckF[E, A] =
      CheckF { a =>
        (this(a), that(a)) match {
          case (Left(l), Left(r))   => Left(Semigroup[E].combine(l, r))
          case (Left(l), Right(r))  => Left(l)
          case (Right(l), Right(r)) => Right(r)
          case (Right(l), Left(r))  => Left(r)
        }
      }

    def and1(that: CheckF[E, A])(implicit s: Semigroup[E]): CheckF[E, A] =
      CheckF { a =>
        (this(a), that(a)) match {
          case (Left(l), Left(r))   => (l |+| r).asLeft
          case (Left(l), Right(r))  => l.asLeft
          case (Right(l), Right(r)) => r.asRight
          case (Right(l), Left(r))  => r.asLeft
        }
      }

    def flatMap[B](f: A => CheckF[E, B]): CheckF[E, B] =
      CheckF { a =>
        (this(a), f(a)) match {
          case Left(
        }
      }
  }
  // CheckF( _ > 10 ) // CheckF[E, Int]
}
