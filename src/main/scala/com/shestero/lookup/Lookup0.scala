package com.shestero.lookup


import scala.collection.mutable
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.util.Try
import scala.util.chaining._

abstract class LookupCached0[K, V, +F[_] <: IterableOnce[_]] extends Lookup0[K, V, F] {
  def clear(): Unit
}

abstract class Lookup0[K, V, +F[_] <: IterableOnce[_]] {
  import Lookup0.pure

  def get: K => F[V]

  def map[W](f: V => W): Lookup0[K, W, IterableOnce] = pure(get andThen { _.iterator.map[W]{ case v: V => f(v) } })

  lazy val cached: LookupCached0[K, V, F] = new LookupCached0[K, V, F] {
    private val cache: mutable.Map[K, F[V]] = mutable.Map.empty

    override def get: K => F[V] = k =>
      cache.synchronized { cache.get(k) getOrElse Lookup0.this.get(k).tap(cache.update(k, _)) }

    override def clear(): Unit = cache.synchronized { cache.clear() }
  }
}

object Lookup0 {

  def apply[K, V, F[_] <: IterableOnce[_]]: (K => F[V]) => Lookup0[K, V, F] = pure[K, V, F]

  def pure[K, V, F[_] <: IterableOnce[_]](f: K => F[V]): Lookup0[K, V, F] = new Lookup0[K, V, F] {
    override def get: K => F[V] = f
  }

  def empty[K, V]: Lookup0[K, V, IterableOnce] = pure[K, V, IterableOnce] { _ => Iterable.empty[V] }

  def const[K, V](always: V): Lookup0[K, V, IterableOnce] = pure[K, V, IterableOnce] { _ => Iterable.single(always) }

  def lazyPure[K, V, F[_] <: IterableOnce[_]](f: => K => F[V]): Lookup0[K, V, F] = new Lookup0[K, V, F] {
    private lazy val lzy: K => F[V] = f
    override def get: K => F[V] = lzy
  }

  def fromFuture[K, V, F[_] <: IterableOnce[_]]
  (
    f: Future[Lookup0[K, V, F]],
    timeout: Duration = Duration.Inf,
    bypass: Lookup0[K, V, F] //= Lookup0.empty[K, V, F]
  ): Lookup0[K, V, F] = new Lookup0[K, V, F] {
    override def get: K => F[V] = Try(Await.result(f, timeout)).toOption.getOrElse(bypass).get
  }

  def fromPureFuture[K, V, F[_] <: IterableOnce[_]]
  (
    f: K => Future[F[V]],
    timeout: Duration = Duration.Inf,
  ): Lookup0[K, F[V], Option] = new Lookup0[K, F[V], Option] {
    override def get: K => Option[F[V]] = f andThen { fut => Try(Await.result(fut, timeout)).toOption }
  }

  def async[K, V, F[_] <: IterableOnce[_]]
  (
    f: => Lookup0[K, V, F],
    timeout: Duration = Duration.Inf,
    bypass: Lookup0[K, V, F] //= Lookup0.empty[K, V, F]
  )(implicit ec: ExecutionContext): Lookup0[K, V, F] = fromFuture[K, V, F](Future(f), timeout, bypass)

  implicit def get[K, V, F[_] <: IterableOnce[_]](f: Lookup0[K, V, F]): K => F[V] = f.get
}

