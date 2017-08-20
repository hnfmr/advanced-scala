package io.hnfmr.chapter3

object ContraMapTut extends App {

  trait Printable[A] {
    def format(value: A): String
    def contramap[B](func: B => A): Printable[B] = {
      val self = this
      new Printable[B] {
        def format(value: B): String = self.format(func(value))
      }
    }
  }

  def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)

  implicit val stringPrintable = new Printable[String] {
    def format(value: String): String =
      "\"" + value + "\""
  }

  implicit val booleanPrintable = new Printable[Boolean] {
    def format(value: Boolean): String =
      if (value) "yes" else "no"
  }

  implicit val intPrintable = new Printable[Int] {
    def format(value: Int): String = s"IntPrintable: $value"
  }

  val a = format("hello")
  val b = format(false)

  final case class Box[A](value: A)

  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
    p.contramap[Box[A]](_.value)

  println(format(Box(false)))
  println(format(Box("Hello")))
  println(format(Box(10)))
}

