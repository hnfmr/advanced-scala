package io.hnfmr.chapter1

import cats.Show
import cats.syntax.show._

object CatShow extends App {
  final case class Cat ( name: String, age: Int, color: String )

  implicit val showCat = new Show[Cat] {
    def show(cat: Cat): String = s"${cat.name} is a ${cat.age} year-old ${cat.color} cat."
  }

  val cat = Cat("Dudu", 1, "Lilac")

  println(cat.show)
}
