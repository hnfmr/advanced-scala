package io.hnfmr.chapter9

import cats.Monoid

object PygmyHadoop extends App {

  def foldMap[A, B : Monoid](seq: Vector[A])(f: A => B): B = ???
}