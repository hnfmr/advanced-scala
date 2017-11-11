package io.hnfmr.chapter10

import cats.Semigroup
import cats.syntax.either._
import cats.syntax.semigroup._
import cats.instances.list._

import cats.data.Validated

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

  println("-"*50)
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

  import Predicate._
  val check5: Predicate[List[String], Int] = Pure {
    a: Int => if (a < 10) Validated.Invalid(List(s"$a is too small, must be >= 10")) else
      Validated.Valid(a)
  }

  val check6: Predicate[List[String], Int] = Pure {
    a: Int => if (a % 2 != 0) Validated.Invalid(List(s"$a is odd, must be even")) else Validated.Valid(a)
  }

  val check7: Predicate[List[String], Int] = And(check5, check6)
  val check8: Predicate[List[String], Int] = Or(check5, check6)

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

  println("-"*50)

  // check predicates and then transform
  val check9 = Check(check7) map ( (a: Int) => (a, "Good"))
  println( check9(7) )
  println( check9(8) )
  println( check9(12) )
  println( check9(13) )

  println("-"*50)
  val check10 = Check(check5) andThen Check(check6)
  println( check10(7) )
  println( check10(8) )
  println( check10(12) )
  println( check10(13) )
}
