package io.hnfmr.chapter7

object FoldableEx extends App {
  def map[A, B](la: List[A])(fn: A => B): List[B] =
    la.foldRight(List.empty[B])(fn(_) :: _)

  def flatMap[A, B](la: List[A])(fn: A => List[B]): List[B] =
    la.foldRight(List.empty[B])(fn(_) ++ _)

  def filter[A](la: List[A])(fn: A => Boolean): List[A] =
    la.foldRight(List.empty[A])((e, a) => {
      if (fn(e)) e :: a
      else a
    })

  def fn1(n: Int): Int = n * 2
  def fn2(n: Int): List[Int] = List(n * 2 - 1, n * 2, n * 2 + 1)

  def fn3(n: Int): Boolean = n % 2 == 0

  println(map(List(1, 2, 3))(fn1))
  println(map(Nil)(fn1))

  println(flatMap(List(1,2,3))(fn2))

  println(filter(List(1,2,3,4,5))(fn3))
}

