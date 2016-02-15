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


object SelectionSort extends Sort {
  override def sort[T: Ordering](arr: Array[T]): Array[T] = {
    val n: Int = arr.length
    for (i <- 0 until n - 1) {
      var idxMin = i
      for (j <- (i + 1) until n) {
        if (arr(j) < arr(idxMin)) {
          idxMin = j
        }
      }
      if (idxMin != i) {
        val t = arr(i)
        arr(i) = arr(idxMin)
        arr(idxMin) = t
      }
    }
    arr
  }
}