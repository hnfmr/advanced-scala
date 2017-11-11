package io.hnfmr.chapter10

import cats.Semigroup
import cats.data.Validated.{Valid, Invalid}
import cats.data.Validated
import cats.syntax.semigroup._

import cats.syntax.apply._

sealed trait Check[E, A, B] {
  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B]

  def map[C](f: B => C): Check[E, A, C] = Map[E, A, B, C](this, f)

  def flatMap[C](f: B => Check[E, A, C]): Check[E, A, C] = FlatMap[E, A, B, C](this, f)

  def andThen[C](next: Check[E, B, C]): Check[E, A, C] = AndThen[E, A, B, C](this, next)
}

object Check {
  def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] =
    PurePredicate(pred)
  def apply[E, A, B](f: A => Validated[E, B]): Check[E, A, B] = Pure(f)
}


final case class Map[E, A, B, C]( check: Check[E, A, B], f: B => C) extends Check[E, A, C] {
  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
    check(a).map(f)
}

final case class PurePredicate[E, A]( pred: Predicate[E, A]) extends Check[E, A, A] {
  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
    pred(a)
}

final case class Pure[E, A, B]( f: A => Validated[E, B]) extends Check[E, A, B] {
  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B] =
    f(a)
}

final case class FlatMap[E, A, B, C]( check: Check[E, A, B], f: B => Check[E, A, C]) extends Check[E, A, C] {
  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
    check(a) match {
      case Valid(x) => f(x)(a)
      case x @ Invalid(_) => x
    }
}

final case class AndThen[E, A, B, C]( left: Check[E, A, B], right: Check[E, B, C]) extends Check[E, A, C] {
  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
    left(a) match {
      case Valid(x) => right(x)
      case x @ Invalid(_) => x
    }
}

