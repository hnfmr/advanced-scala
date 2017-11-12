package io.hnfmr.chapter10

import cats.Semigroupal
import cats.syntax.validated._

import Functions._

object UserInputValidation extends App {

  val userNameCheck: Check[Errors, String, String] =
    Check(
      longerThan(4) and alphanumeric
    )

  val checkLeft: Check[Errors, String, String] = Check(longerThan(0))
  val checkRight: Check[Errors, String, String] = Check(longerThan(3) and containsOnce('.'))

  def splitCheck: Check[Errors, String, (String, String)] =
    Check { s =>
      s.split("@") match {
        case Array(first, second) => (first, second).validNel[String]
        case _ => "Must contain a single @ character".invalidNel[(String, String)]
      }
    }

  val emailCheck: Check[Errors, String, String] =
    splitCheck andThen Check { _ match {
      case (f, s) => Semigroupal.map2(checkLeft(f), checkRight(s))( _+ "@" + _)
    }}

  println("-"*50)
  println(emailCheck("what@at.com"))
  println(emailCheck("what@at..com"))
  println(emailCheck("@at..com"))

}
