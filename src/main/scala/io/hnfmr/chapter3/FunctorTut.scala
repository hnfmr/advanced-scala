package io.hnfmr.chapter3

import cats.Functor
import cats.syntax.functor._

object FunctorTut extends App {
  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  implicit val treeFunctor = new Functor[Tree] {
    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
      tree match {
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
        case Leaf(v) => Leaf(f(v))
      }
    }
  }

  val tree1: Tree[Int] = Branch(
    Branch(Leaf(5), Leaf(6)),
    Leaf(11)
  )

  val tree2 = tree1.map( (x) => x*x )
  println(tree2)

  branch(leaf(10), leaf(20)).map(_ * 2)

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](value: A): Tree[A] = Leaf(value)

}