package io.hnfmr.tagless

import scala.language.higherKinds

object TaglessTest extends App {
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

  // builds an expression like: println(s"$text ${a + (b + 1)}")
  def buildComplexExpression(text: String, a: Int, b: Int) = new ScalaToLanguageBridge[String] {
    override def apply[Wrapper[_]](implicit F: Language[Wrapper]): Wrapper[String] = {
      val addition = F.add(F.number(a), F.increment(F.number(b)))
      F.concat(F.text(text), F.toString(addition))
    }
  }

  val fullExpression = buildComplexExpression("Result is ", 10, 1)

  type Id[A]= A

  val interpret = new Language[Id] {
    override def number(v: Int): Id[Int] = v
    override def increment(a: Id[Int]): Id[Int] = a + 1
    override def add(a: Id[Int], b: Id[Int]): Id[Int] = a + b

    override def text(v: String): Id[String] = v
    override def toUpper(a: Id[String]): Id[String] = a.toUpperCase
    override def concat(a: Id[String], b: Id[String]): Id[String] = a + " " + b

    override def toString(v: Id[Int]): Id[String] = v.toString
  }

  println(s"interpreted in full : ${fullExpression.apply(interpret)}")

  // pretty print
  type PrettyPrint[T] = String

  val interpretAsPrettyPrint = new Language[PrettyPrint] {
    override def number(v: Int): PrettyPrint[Int] = s"($v)"
    override def increment(a: PrettyPrint[Int]): PrettyPrint[Int] = s"(inc $a)"
    override def add(a: PrettyPrint[Int], b: PrettyPrint[Int]): PrettyPrint[Int] = s"(+ $a $b)"

    override def text(v: String): PrettyPrint[String] = s"[$v]"
    override def toUpper(a: PrettyPrint[String]): PrettyPrint[String] = s"(toUpper $a)"
    override def concat(a: PrettyPrint[String], b: PrettyPrint[String]): PrettyPrint[String] = s"(concat $a $b)"

    override def toString(v: PrettyPrint[Int]): PrettyPrint[String] = s"(toString $v)"
  }

  println(s"interpreted in pretty print : ${fullExpression.apply(interpretAsPrettyPrint)}")
}