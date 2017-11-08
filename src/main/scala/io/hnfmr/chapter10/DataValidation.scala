package io.hnfmr.chapter10

import cats.Semigroup
import cats.syntax.either._

object DataValidation extends App {

  final case class CheckF[E, A](f: A => Either[E, A]) {
    def apply(a: A): Either[E, A] = f(a)

    def and[E: Semigroup](that: CheckF[E, A]): CheckF[E, A] = ???
  }
}
