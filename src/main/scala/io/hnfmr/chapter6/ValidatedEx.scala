package io.hnfmr.chapter6

import cats.data.Validated
import cats.syntax.either._
import cats.syntax.cartesian._
import cats.instances.list._

object ValidatedEx extends App {
  case class User(name: String, age: Int)

  type FormData = Map[String, String]
  type ErrorsOr[A] = Either[List[String], A]
  type AllErrorsOr[A] = Validated[List[String], A]

  def getValue(k: String)(m: FormData): ErrorsOr[String] =
    m.get(k).toRight(List(s"$k field is not specified"))

  val getName =getValue("name") _
//  val getAge = getValue("age")

  def parseInt(n: String)(data: String): ErrorsOr[Int] =
    Right(data)
      .flatMap( s => Either.catchOnly[NumberFormatException](s.toInt) )
      .leftMap( _ => List(s"$n must be an integer"))

  def nonBlank(n: String)(data: String): ErrorsOr[String] =
    Right(data).ensure(List(s"$n cannot be blank"))(_.nonEmpty)

  def nonNegative(n: String)(data: Int): ErrorsOr[Int] =
    Right(data).ensure(List(s"$n must be non-negative"))(_ >= 0)

  def readName(data: FormData): ErrorsOr[String] =
    getValue("name")(data).flatMap(nonBlank("name"))

  def readAge(data: FormData): ErrorsOr[Int] =
    getValue("age")(data)
      .flatMap(nonBlank("age"))
      .flatMap(parseInt("age"))
      .flatMap(nonNegative("age"))

  def readUser(data: FormData): AllErrorsOr[User] =
    (
      readName(data).toValidated |@|
      readAge(data).toValidated
    ).map(User.apply)

  println(readUser(Map(
    "name" -> "",
    "age" -> "-1"
  )))

  println(readUser(Map(
    "name" -> "",
    "age" -> "-18"
  )))

  println(readUser(Map(
    "name" -> "Garfield",
    "age" -> "2"
  )))
}
