package com.shestero.core

import java.io.InputStream

object InputStreamOps {

  implicit private class IteratorOps(it: Iterator[Int]) { // Iterator[Byte| -1]
    def inputStream: InputStream =
      new InputStream {
        override def read(): Int = {
          if (it.hasNext) it.next()
          else -1
        }
      }
  }

  implicit class InputStreamOps(is: InputStream) {

    /** convert InputStream to Iterator */
    def iterator: Iterator[Int] = // Iterator[Byte| -1]
      Iterator.continually(is.read).takeWhile(_ != -1)

    /** duplicate InputStream */
    def duplicate: (InputStream, InputStream) = {
      val (it1, it2) = is.iterator.duplicate
      (it1.inputStream, it2.inputStream)
    }
  }
}
