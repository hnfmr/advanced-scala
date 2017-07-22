package io.hnfmr.chapter4

import cats.Eval

object EvalTut extends App {
  def factorial(n: BigInt): Eval[BigInt] = {
    if (n == 1) Eval.now(n)
    else Eval.defer(factorial(n-1).map(_ * n))
  }

  println(factorial(50).value)

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail => Eval.defer( fn(head, foldRight(tail, acc)(fn) ) )
      case Nil => Eval.now(acc)
    }

  println(foldRight(List(1,2,3,4), 2)( (a: Int, b: Eval[Int]) => {
    for {
      be <- b
    } yield a + be
  }).value)
}
