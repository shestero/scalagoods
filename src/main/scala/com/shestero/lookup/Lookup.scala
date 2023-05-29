package com.shestero.lookup

import scala.collection.mutable
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.util.chaining._
import scala.util.Try


trait Lookup[K, O] {
  def get: K => Option[O]

  def ++[OO <: O](other: Lookup[K, OO]): Lookup[K, O] = new Lookup[K, O] {
    override def get: K => Option[O] = k => Lookup.this.get(k) orElse other.get(k)
  }

  def orElse[OO <: O](other: => Lookup[K, OO]): Lookup[K, O] = new Lookup[K, O] {
    override def get: K => Option[O] = k => Lookup.this.get(k) orElse other.get(k)
  }

  def withDefault(default: O): Lookup[K, O] = this ++ Lookup.const(default)

  def map[T](f: O => T): Lookup[K, T] = Lookup.pure(get andThen { _.map(f) })

  def cached: Lookup[K, O] = new Lookup[K, O] {
    private val cache: mutable.Map[K, O] = mutable.Map.empty
    override def get: K => Option[O] = k =>
      synchronized { cache.get(k) orElse Lookup.this.get(k).tap(_.foreach(v => cache.update(k, v))) }
  }
}

object Lookup {

  def empty[K, O]: Lookup[K, O] = new Lookup[K, O] {
    override def get: K => Option[O] = _ => None
  }

  def const[K, O](always: O): Lookup[K, O] = new Lookup[K, O] {
    override def get: K => Option[O] = _ => Some(always)
  }

  def pure[K, O](f: K => Option[O]): Lookup[K, O] = new Lookup[K, O] {
    override def get: K => Option[O] = f
  }

  def lazyPure[K, O](f: => K => Option[O]): Lookup[K, O] = new Lookup[K, O] {
    private lazy val l: K => Option[O] = f
    override def get: K => Option[O] = l
  }

  def fromMap[K, O](m: Map[K, O]): Lookup[K, O] = pure(m.get)

  def fromIterator[K, O](k: O => K): Iterator[O] => Lookup[K, O] = fromMap[K, O] _ compose { _.map(o => k(o) -> o).toMap }
  def fromIterable[K, O](k: O => K): Iterable[O] => Lookup[K, O] = fromMap[K, O] _ compose { _.map(o => k(o) -> o).toMap }

  def fromFuture[K, O](
                        f: Future[Lookup[K, O]],
                        timeout: Duration = Duration.Inf,
                        bypass: Lookup[K, O] = Lookup.empty[K, O]
                      ): Lookup[K, O] = new Lookup[K, O] {
    override def get: K => Option[O] = Try(Await.result(f, timeout)).toOption.getOrElse(bypass).get
  }

  def async[K, O](
                   f: => Lookup[K, O],
                   timeout: Duration = Duration.Inf,
                   bypass: Lookup[K, O] = Lookup.empty[K, O]
                 )(implicit ec: ExecutionContext): Lookup[K, O] = fromFuture(Future(f), timeout, bypass)

  implicit def get[K, O](f: Lookup[K, O]): K => Option[O] = f.get
}
