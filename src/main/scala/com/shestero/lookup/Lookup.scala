package com.shestero.lookup

import scala.language.implicitConversions
import scala.collection.{MapView, mutable}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.util.chaining._
import scala.util.Try


trait LookupCached[K, V] extends Lookup[K, V] {
  def clear(): Unit
}

trait Lookup[K, V] {
  import Lookup.pure

  def get: K => Option[V]

  val getSeq: Seq[K] => Seq[V] = _.flatMap(get)

  def filter(pred: K => Boolean): Lookup[K, V] = pure { k => Option.when(pred(k))(get(k)).flatten }

  val filterNot: (K => Boolean) => Lookup[K, V] = _ andThen { !_ } pipe filter

  val getOrElse: (K, => V) => V = get(_).getOrElse(_)

  def ++(other: Lookup[K, V]): Lookup[K, V] = pure { k => get(k) orElse other.get(k) }

  def orElse(other: => Lookup[K, V]): Lookup[K, V] = pure { k => get(k) orElse other.get(k) }

  val withDefault: V => Lookup[K, V] = this ++ Lookup.const(_)

  def flatMap[W](f: V => Option[W]): Lookup[K, W] = pure{ get andThen { _.flatMap(f) } }

  def map[W](f: V => W): Lookup[K, W] = pure{ get andThen { _.map(f) } }

  def contramap[L](g: L => K): Lookup[L, V] = pure { g andThen get }

  def imap[W, L]: (V => W, L => K) => Lookup[L, W] = map(_) contramap(_)

  def andThen[W]: Lookup[V, W] => Lookup[K, W] = _.get pipe flatMap

  def compose[L]: Lookup[L, K] => Lookup[L, V] = _.flatMap(get)

  lazy val cached: LookupCached[K, V] = new LookupCached[K, V] {
    private val cache: mutable.Map[K, Option[V]] = mutable.Map.empty
    override def get: K => Option[V] = k =>
      cache.synchronized { cache.get(k) getOrElse Lookup.this.get(k).tap(cache.update(k, _)) }
    override def clear(): Unit = cache.synchronized { cache.clear() }
  }
}

object Lookup {

  def pure[K, V](f: K => Option[V]): Lookup[K, V] = new Lookup[K, V] {
    override def get: K => Option[V] = f
  }

  def apply[K, V]: (K => Option[V]) => Lookup[K, V] = pure

  def empty[K, V]: Lookup[K, V] = pure { _ => None }

  def const[K, V](always: V): Lookup[K, V] = pure { _ => Some(always) }

  def lazyPure[K, V](f: => K => Option[V]): Lookup[K, V] = new Lookup[K, V] {
    private lazy val lzy: K => Option[V] = f
    override def get: K => Option[V] = lzy
  }

  def fromMap[K, V](m: Map[K, V]): Lookup[K, V] = pure(m.get)
  def fromMapView[K, V]: MapView[K, V] => Lookup[K, V] = fromMap[K, V] _ compose { _.toMap }
  def fromIterator[K, V](k: V => K): Iterator[V] => Lookup[K, V] = fromMap[K, V] _ compose { _.map(o => k(o) -> o).toMap }
  def fromIterable[K, V](k: V => K): Iterable[V] => Lookup[K, V] = fromMap[K, V] _ compose { _.map(o => k(o) -> o).toMap }

  def fromFuture[K, V](
                        f: Future[Lookup[K, V]],
                        timeout: Duration = Duration.Inf,
                        bypass: Lookup[K, V] = empty[K, V]
                      ): Lookup[K, V] = new Lookup[K, V] {
    override def get: K => Option[V] = Try(Await.result(f, timeout)).toOption.getOrElse(bypass).get
  }

  def fromPureFuture[K, V](
                            f: K => Future[Option[V]],
                            timeout: Duration = Duration.Inf,
                          ): Lookup[K, V] = new Lookup[K, V] {
    override def get: K => Option[V] = f andThen { fut => Try(Await.result(fut, timeout)).toOption.flatten }
  }

  def async[K, V](
                   f: => Lookup[K, V],
                   timeout: Duration = Duration.Inf,
                   bypass: Lookup[K, V] = empty[K, V]
                 )(implicit ec: ExecutionContext): Lookup[K, V] = fromFuture(Future(f), timeout, bypass)

  implicit def get[K, V](f: Lookup[K, V]): K => Option[V] = f.get

}
