package com.shestero.sql

import java.sql.ResultSet

object ResultSetOps {

  implicit class ResultSetOps(rs: ResultSet) {

    def map[T](f: ResultSet => T): Seq[T] = mapUnsafe(f).toSeq

    def map[T1, T2](f1: ResultSet => T1, f2: ResultSet => T2): Seq[(T1,T2)] = mapUnsafe(f1, f2).toSeq

    def mapUnsafe[T](f: ResultSet => T): Iterator[T] =
      Iterator.continually(rs).takeWhile(_.next()).map(f)

    def mapUnsafe[T1, T2](f1: ResultSet => T1, f2: ResultSet => T2): Iterator[(T1,T2)] =
      Iterator.continually(rs).takeWhile(_.next()).map(rs => (f1(rs), f2(rs)))
  }

}
