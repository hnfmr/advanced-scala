package io.hnfmr.chapter11

import cats.Monoid
import cats.instances.int._
import cats.instances.list._
import cats.instances.map._
import cats.syntax.semigroup._
import cats.syntax.foldable._

import scala.language.higherKinds

object GCounterTut extends App {

  val g1 = Map("a" -> 7, "b" -> 3)
  val g2 = Map("a" -> 2, "b" -> 5)

  implicit def mapInstance[K, V]: CRDTCounter[Map, K, V] =
    new CRDTCounter[Map, K, V] {
      override def increment(f: Map[K, V])(k: K, v: V)(implicit m: Monoid[V]): Map[K, V] =
        f + (k -> f.getOrElse(k, m.empty))

      def merge(f1:Map[K, V], f2: Map[K, V])(implicit b: BoundedSemiLattice[V]): Map[K, V] =
        f1 |+| f2

      override def total(f: Map[K, V])(implicit m: Monoid[V]): V = f.values.toList.combineAll
    }

  val counter = CRDTCounter[Map, String, Int]

  val merged = counter.merge(g1, g2)

  val total = counter.total(merged)

  println(total)

  //-----------------KeyValueStore
  implicit val mapKVS: KeyValueStore[Map] =
    new KeyValueStore[Map] {
      override def values[K, V](f: Map[K, V]): List[V] = f.values.toList

      override def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] = f + (k -> v)

      override def get[K, V](f: Map[K, V])(k: K): Option[V] = f.get(k)

      override def getOrElse[K, V](f: Map[K, V])(k: K, default: V): V =
        f.getOrElse(k, default)
    }

  import KeyValueStore._

  implicit def gcounterInstance[F[_,_], K, V](implicit kvs: KeyValueStore[F], km: Monoid[F[K, V]]) =
    new CRDTCounter[F, K, V] {
      override def increment(f: F[K, V])(k: K, v: V)(implicit m: Monoid[V]): F[K, V] =
        f.put(k, f.getOrElse(k, m.empty))

      override def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V] =
        f1 |+| f2

      override def total(f: F[K, V])(implicit m: Monoid[V]): V = f.values.combineAll
    }
}
