package com.shestero.core

import java.util.concurrent.Phaser

object IteratorOps {

  implicit class IteratorOps[+A](it: Iterator[A]) {

    def drain(): Unit = it.foreach(_ => ())

    def fork(threads: Int): Iterator[A] = new Phaser(threads) with Iterator[A] {

      private val multiplied: Iterator[A] = it.flatMap(Iterator.fill(threads)(_))

      override def hasNext: Boolean = it.synchronized(it.hasNext)

      override def next(): A = {
        arriveAndAwaitAdvance()
        val next = multiplied.synchronized(multiplied.next())
        arriveAndAwaitAdvance()
        next
      }
    }

  }

}