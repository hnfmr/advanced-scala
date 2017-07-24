package io.hnfmr.chapter4

import cats.data.State
import cats.syntax.applicative._

object StateTut extends App {

  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case a   => operand(a.toInt)
    }

  def operand(num: Int): CalcState[Int] =
    State[List[Int], Int] { stack => (num :: stack, num)}

  def operator(func: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case a :: b :: tail =>
        val ans = func(a, b)
        (ans :: tail, ans)
      case _ => sys.error("Impossible!")
    }

  val program = for {
    _ <- evalOne("1")
    _ <- evalOne("3")
    ans <- evalOne("/")
  } yield ans

  println(program.runA(List()).value)

  def evalAll(input: List[String]): CalcState[Int] = {
    val i = 0.pure[CalcState]
    input.foldLeft(i) { (a, b) => {
      a flatMap (_ => evalOne(b) )
    }}
  }

  println(evalAll(List("1", "2", "+")).runA(Nil).value)
}
