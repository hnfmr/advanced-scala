package io.hnfmr.chapter1

object PrintableTut extends App {

  // the type class: a generic trait
  trait Printable[A] {
    def format(a: A): String
  }

  // instances for each type we care about
  object PrintableInstances {
    implicit val intPrintable = new Printable[Int] {
      def format(a: Int): String = {
        a.toString
      }
    }

    implicit val stringPrintable = new Printable[String] {
      def format(a: String): String = a
    }

    implicit val catPrintable = new Printable[Cat] {
      def format(cat: Cat): String = s"${cat.name} is a ${cat.age} year-old ${cat.color} cat."
    }
  }

  // one or more generic interface methods
  // method 1: interface object
  object Printable {
    def format[A](a: A)(implicit p: Printable[A]): String = p.format(a)
    def print[A](a: A)(implicit p: Printable[A]): Unit = println(p.format(a))
  }

  final case class Cat ( name: String, age: Int, color: String )

  val cat = Cat("Dudu", 1, "Lilac")

  import PrintableInstances._
  Printable.print(cat)

  // method 2: interface syntax (the most common way: implicit classes)
  object PrintableSyntax {
    implicit class PrintOps[A](value: A) {
      def format(implicit p: Printable[A]): String = p.format(value)
      def print(implicit p: Printable[A]): Unit = println(format)
    }
  }

  import PrintableSyntax._
  cat.print
}