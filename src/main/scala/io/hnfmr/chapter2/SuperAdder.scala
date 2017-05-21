package io.hnfmr.chapter2

import cats.Monoid
import cats.syntax.monoid._
import cats.syntax.option._
import cats.instances.int._
import cats.instances.option._

object SuperAdder extends App {
  def add[A](items: List[A])(implicit monoid: Monoid[A]): A= {
    items.foldLeft(monoid.empty)( _ |+| _)
  }

  val a = add(List(1,2,3,4))
  val b = add(List(Some(1), None, Some(2)))

  // two ways to ask the compiler to select Option[Int] instead of Some[Int]
  // since Some[Int] is a subtype of Option[Int]
  val c = add(List(Some(1):Option[Int], Some(2):Option[Int]))
  val cc = add(List(1.some, 2.some))

  case class Order(totalCost: Double, quantity: Double)

  implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
    def combine(a: Order, b: Order): Order = Order(
      totalCost = a.totalCost + b.totalCost,
      quantity = a.quantity + b.quantity
    )
    def empty: Order = Order(totalCost = 0.0d, quantity = 0.0d)
  }

  val d = add(List(Order(1.1, 2.2), Order(2.2, 3.3)))

  // currently Cats has no mechanism for selecting alternative instances
  // e.g.
}
