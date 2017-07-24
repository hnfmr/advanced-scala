package io.hnfmr.chapter4

import cats.Monad
import scala.annotation.tailrec
import cats.syntax.functor._
import cats.syntax.flatMap._

object CustomMonads extends App {
  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)

  implicit val treeMonad =
    new Monad[Tree] {
      def flatMap[A, B](tree: Tree[A])(fn: A => Tree[B]): Tree[B] =
        tree match {
          case Branch(l, r) => Branch(flatMap(l)(fn), flatMap(r)(fn))
          case Leaf(v) => fn(v)
        }

      def pure[A](v: A): Tree[A] = leaf(v)

      def tailRecM[A, B](a: A)(fn: A => Tree[Either[A,B]]): Tree[B] =
        fn(a) match {
          case Branch(l, r) =>
            Branch(
              flatMap(l) {
                case Left(ll) => tailRecM(ll)(fn)
                case Right(ll) => pure(ll)
              },
              flatMap(r) {
                case Left(rr) => tailRecM(rr)(fn)
                case Right(rr) => pure(rr)
              }
            )
          case Leaf(Left(value)) => tailRecM(value)(fn)
          case Leaf(Right(value)) => Leaf(value)
        }
    }

  val t1 = branch(leaf(100), leaf(200)).flatMap( x => branch(leaf(x - 1), leaf(x + 1)))
  println(t1)

  val t2 = for {
    a <- branch(leaf(100), leaf(200))
    b <- branch(leaf(a - 10), leaf(a + 10))
    c <- branch(leaf(b - 1), leaf(b + 1))
  } yield c

  println(t2)
}
