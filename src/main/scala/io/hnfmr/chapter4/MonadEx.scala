package io.hnfmr.chapter4

import cats.Id

object MonadEx extends App {

  def pure[A](value: A): Id[A] = value
  def map[A, B](ia: Id[A])(f: A => B): Id[B] = f(ia)
  def flatMap[A, B](ia: Id[A])(f: A => Id[B]): Id[B] = f(ia)

}
