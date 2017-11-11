package io.hnfmr.chapter10

import cats.Semigroupal
import cats.data.{NonEmptyList, OneAnd, Validated}
import cats.instances.list._
import cats.syntax.cartesian._
import cats.syntax.validated._

object UserInputValidation extends App {

  type Errors = NonEmptyList[String]

  def error(s: String): NonEmptyList[String] =
    NonEmptyList(s, Nil)

  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be longer than $n characters"),
      str => str.size > n)

  val alphanumeric: Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be all alphanumeric characters"),
      str => str.forall(_.isLetterOrDigit))

  def contains(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char"),
      str => str.contains(char))

  def containsOnce(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char only once"),
      str => str.filter(c => c == char).size == 1)

  val userNameCheck: Check[Errors, String, String] =
    Check(
      longerThan(4) and alphanumeric
    )

  val checkLeft: Check[Errors, String, String] = Check(longerThan(0))
  val checkRight: Check[Errors, String, String] = Check(longerThan(3) and containsOnce('.'))

  import Predicate._

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
}
