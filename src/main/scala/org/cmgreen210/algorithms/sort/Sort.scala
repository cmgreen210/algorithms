package org.cmgreen210.algorithms

import scala.math.Ordered._

trait Sort {
  def sort[T: Ordering](arr: Array[T]): Array[T]
}

object InsertionSort extends Sort {

  override def sort[T: Ordering](arr: Array[T]): Array[T] ={
    val n: Int = arr.length
    for (i <- 0 until n) {
      var j = i
      while (j > 0 && arr(j-1) > arr(j)) {
        val t = arr(j-1)
        arr(j-1) = arr(j)
        arr(j) = t
        j -= 1
      }
    }
    arr
  }
}