package io.hnfmr.free

import cats.free.FreeApplicative
import cats.free.FreeApplicative.lift

import cats.implicits._
import cats.Id
import cats.arrow.FunctionK

object FApplicative extends App {
  sealed abstract class ValidationOp[A]
  case class Size(size: Int) extends ValidationOp[Boolean]
  case object HasNumber extends ValidationOp[Boolean]

  type Validation[A] = FreeApplicative[ValidationOp, A]

  def size(s: Int): Validation[Boolean] = lift(Size(s))
  val hasNumber: Validation[Boolean] = lift(HasNumber)

  val prog: Validation[Boolean] = (size(5), hasNumber).mapN { case (l, r) => l && r }

  // a function that takes a string as input
  type FromString[A] = String => A

  val compiler = new FunctionK[ValidationOp, FromString] {
    def apply[A](fa: ValidationOp[A]): FromString[A] = str =>
      fa match {
        case Size(s)   => str.size >= s
        case HasNumber => str.exists(c => "0123456789".contains(c))
      }
  }

  val validator = prog.foldMap[FromString](compiler)

  println(validator("1234"))
  println(validator("1234678"))
}