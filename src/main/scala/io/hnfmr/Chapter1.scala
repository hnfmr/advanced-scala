object Chapter1 extends App {
  trait Printable[A] {
    def format(a: A): String
  }

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

  object Printable {
    def format[A](a: A)(implicit p: Printable[A]): String = p.format(a)
    def print[A](a: A)(implicit p: Printable[A]): Unit = println(p.format(a))
  }

  final case class Cat ( name: String, age: Int, color: String )

  val cat = Cat("Dudu", 1, "Lilac")

  import PrintableInstances._
  Printable.print(cat)

  object PrintableSyntax {
    implicit class PrintOps[A](value: A) {
      def format(implicit p: Printable[A]): String = p.format(value)
      def print(implicit p: Printable[A]): Unit = println(format)
    }
  }

  import PrintableSyntax._
  cat.print
}