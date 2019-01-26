package io.hnfmr.tagless

import scala.language.higherKinds

trait Language[Wrapper[_]] {
  def number(v: Int): Wrapper[Int]
  def increment(a: Wrapper[Int]): Wrapper[Int]
  def add(a: Wrapper[Int]): Wrapper[Int]

  def text(v: String): Wrapper[String]
  def toUpper(a: Wrapper[String]): Wrapper[String]
  def concat(a: Wrapper[String], b: Wrapper[String]): Wrapper[String]

  def toString(v: Wrapper[Int]): Wrapper[String]
}

trait ScalaToLanguageBridge[ScalaValue] {
  def apply[Wrapper[_]](implicit L: Language[Wrapper]): Wrapper[ScalaValue]
}
