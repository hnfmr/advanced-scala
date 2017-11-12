package io.hnfmr.chapter10

import cats.Semigroup
import cats.data.Validated.{Valid, Invalid}
import cats.data.Validated
import cats.syntax.semigroup._
import cats.syntax.validated._

import cats.syntax.apply._

sealed trait Predicate[E, A] {

  import Predicate._

  def and(that: Predicate[E, A]): Predicate[E, A] =
    And(this, that)

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
    this match {
      case Pure(f) => f(a)
      case And(l, r) => (l(a), r(a)).mapN((_, _) => a)
      case Or(l, r) => (l(a), r(a)) match {
        case (Valid(_), Valid(y)) => Valid(y)
        case (Valid(x), Invalid(_)) => Valid(x)
        case (Invalid(x), Invalid(y)) => Invalid(x |+| y)
        case (Invalid(_), Valid(y)) => Valid(y)
      }
    }

  def run(implicit s: Semigroup[E]): A => Either[E, A] = {
    a =>
      this (a) match {
        case Valid(x) => Right(x)
        case Invalid(x) => Left(x)
      }
  }
}

object Predicate {
  final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]
  final case class Pure[E, A](f: A => Validated[E, A]) extends Predicate[E, A]
  final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

  def apply[E, A](f: A => Validated[E, A]): Predicate[E, A] = Pure(f)

  def lift[E, A](error: E, f: A => Boolean): Predicate[E, A] =
    Pure(a => if (f(a)) a.valid else error.invalid)
}
