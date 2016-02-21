package org.cmgreen210.algorithms

import scala.math.Ordered._

trait Sort {
  def sort[T: Ordering](arr: Array[T]): Array[T]

  def swap[T: Ordering](arr: Array[T], i: Int, j: Int): Unit = {
    val t = arr(i)
    arr(i) = arr(j)
    arr(j) = t
  }
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

object MergeSort extends Sort {
  override def sort[T: Ordering](arr: Array[T]): Array[T] = {
    val workspace = arr.clone
    split(arr, 0, arr.length, workspace)
    arr
  }

  private def split[T: Ordering](arr: Array[T], iBegin: Int,
                                 iEnd: Int, workspace: Array[T]): Unit = {
    if (iEnd - iBegin < 2) return
    val iMid: Int = (iEnd + iBegin) / 2
    split(arr, iBegin, iMid, workspace)
    split(arr, iMid, iEnd, workspace)
    merge(arr, iBegin, iMid, iEnd, workspace)
    copy(workspace, iBegin, iEnd, arr)
  }

  private def merge[T: Ordering](arr: Array[T], iBegin: Int, iMid: Int,
                                 iEnd: Int, workspace: Array[T]): Unit = {
    var l = iBegin
    var r = iMid

    for (i <- iBegin until iEnd) {
      if (l < iMid && (r >= iEnd || arr(l) <= arr(r))) {
        workspace(i) = arr(l)
        l += 1
      } else {
        workspace(i) = arr(r)
        r += 1
      }
    }
  }

  private def copy[T: Ordering](workspace: Array[T], iBegin: Int, iEnd: Int,
                                arr: Array[T]): Unit = {
    for (k <- iBegin until iEnd) {
      arr(k) = workspace(k)
    }
  }
}

object QuickSort extends Sort {
  override def sort[T: Ordering](arr: Array[T]): Array[T] = {
    quicksort(arr, 0, arr.length - 1)
    arr
  }

  private def quicksort[T: Ordering](arr: Array[T],
                                     left: Int, right: Int): Unit = {
    if (left < right) {
      val p = partition(arr, left, right)
      quicksort(arr, left, p)
      quicksort(arr, p + 1, right)
    }
  }

  private def partition[T: Ordering](arr: Array[T],
                                     left: Int, right: Int): Int = {
    val pivot = arr(left)
    var i = left - 1
    var j = right + 1
    while (true) {
      do {
        i += 1
      } while (arr(i) < pivot)
      do {
        j -= 1
      } while (arr(j) > pivot)
      if (i >= j)
        return j
      swap(arr, i, j)
    }
    -1 // shouldn't happen!
  }
}