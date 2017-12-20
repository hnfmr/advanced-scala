package io.hnfmr.free

import cats.free.FreeApplicative
import cats.free.FreeApplicative.lift
import cats.implicits._
import cats.~>
import cats.data.Const
import cats.data.Kleisli
import cats.data.Tuple2K

import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

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

  val compiler = new (ValidationOp ~> FromString) {
    def apply[A](fa: ValidationOp[A]): FromString[A] = str =>
      fa match {
        case Size(s)   => str.size >= s
        case HasNumber => str.exists(c => "0123456789".contains(c))
      }
  }

  val validator = prog.foldMap[FromString](compiler)

  println(validator("1234"))
  println(validator("1234678"))

  // Parallelism
  type ParValidator[A] = Kleisli[Future, String, A]

  //  val parCompiler = new FunctionK[ValidationOp, ParValidator]
  val parCompiler = new (ValidationOp ~> ParValidator) {
    def apply[A](fa: ValidationOp[A]): ParValidator[A] = Kleisli { str =>
      fa match {
        case Size(s) => Future { str.size >= s }
        case HasNumber => Future { str.exists(c => "0123456789".contains(c)) }
      }
    }
  }

  val parValidator = prog.foldMap[ParValidator](parCompiler)
  val res0 = parValidator("299292943943")
  println(Await.result(res0, 2.seconds))

  val res1 = parValidator("299")
  println(Await.result(res1, 2.seconds))

  // Logging
  type Log[A] = Const[List[String], A]

  val logCompiler = new (ValidationOp ~> Log) {
    def apply[A](fa: ValidationOp[A]): Log[A] =
      fa match {
        case Size(s) => Const(List(s"size >= $s"))
        case HasNumber => Const(List("has number"))
      }
  }

  def logValidation[A](validation: Validation[A]): List[String] =
    validation.foldMap[Log](logCompiler).getConst

  println(logValidation(prog))
  println(logValidation(size(5) *> hasNumber *> size(10)))
  println(logValidation((hasNumber, size(3)).mapN(_ || _)))

  // Compose two set of operations
  type ValidationAndLog[A] = Tuple2K[ParValidator, Log, A]

  val prodCompiler: (ValidationOp ~> ValidationAndLog) = parCompiler and logCompiler

  println("-"*50)
  val prodValidation = prog.foldMap[ValidationAndLog](prodCompiler)
  val fst = Await.result(prodValidation.first("222"), 1.second)
  println(fst)
  println(prodValidation.second)
  println("-"*50)
}
