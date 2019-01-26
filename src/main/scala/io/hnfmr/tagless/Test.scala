package io.hnfmr.tagless

import scala.language.higherKinds

object Test extends App {
  def buildNumber(number: Int) = new ScalaToLanguageBridge[Int] {
    override def apply[Wrapper[_]](implicit L: Language[Wrapper]): Wrapper[Int] = L.number(number)
  }

  def buildIncrementNumber(number: Int) = new ScalaToLanguageBridge[Int] {
    override def apply[Wrapper[_]](implicit L: Language[Wrapper]): Wrapper[Int] = L.increment(L.number(number))
  }

  def buildIncrementExpression(expression: ScalaToLanguageBridge[Int]) =
    new ScalaToLanguageBridge[Int] {
      override def apply[Wrapper[_]](implicit L: Language[Wrapper]): Wrapper[Int] = L.increment(expression.apply)
    }
}