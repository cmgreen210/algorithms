package org.cmgreen210.algorithms.sorttest

import org.cmgreen210.algorithms._
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.SpanSugar._
import org.scalatest._

import scala.math.Ordered._
import scala.util.Random

object SortTestUtils {
  def isSorted[T: Ordering](arr: Array[T]): Boolean = {
    for ( i <- 0 until (arr.length - 1)) {
      if ( arr(i) > arr(i + 1)) {
        return false
      }
    }
    true
  }
}

class InsertionSortTest extends FlatSpec with Matchers {

  it should "do insertion sort!" in {

    var arr: Array[Int] = Array.fill(10000)(Random.nextInt)
    Sort.insertionSort(arr)
    assert(SortTestUtils.isSorted(arr))

    arr = Array.fill(1)(Random.nextInt)
    Sort.insertionSort(arr)
    assert(SortTestUtils.isSorted(arr))

    assert(false)
  }
}
