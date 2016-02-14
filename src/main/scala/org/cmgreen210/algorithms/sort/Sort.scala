package org.cmgreen210.algorithms

import scala.math.Ordered._

object Sort {

  def insertionSort[T: Ordering](arr: Array[T]): Unit = {
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
  }

  def mergeSort[T: Ordering](arr: Array[T]): Unit = {

  }
}