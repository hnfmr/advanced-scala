package io.hnfmr.chapter11

import scala.language.higherKinds

trait KeyValueStore[F[_,_]] {
  def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]
  def get[K, V](f: F[K, V])(k: K): Option[V]

  def getOrElse[K, V](f: F[K, V])(k: K, default: V): V =
    get(f)(k).getOrElse(default)

  def values[K, V](f: F[K, V]): List[V]
}

object KeyValueStore {
  implicit class KvsOps[F[_,_], K, V](f: F[K, V]) {
    def put(k: K, v: V)(implicit kvs: KeyValueStore[F]): F[K, V] =
      kvs.put(f)(k, v)

    def get(k: K)(implicit kvs: KeyValueStore[F]): Option[V] =
      kvs.get(f)(k)

    def getOrElse(k: K, default: V)(implicit kvs: KeyValueStore[F]): V =
      kvs.getOrElse(f)(k, default)

    def values(implicit kvs: KeyValueStore[F]): List[V] =
      kvs.values(f)
  }
}