package org.cmgreen210.algorithms.sorttest

import org.cmgreen210.algorithms._
import org.cmgreen210.algorithms.sorttest.SortTestUtils._
import org.scalatest.FlatSpec
import org.scalatest.concurrent.Timeouts
import org.scalatest.exceptions.TestFailedDueToTimeoutException
import org.scalatest.time.SpanSugar._

import scala.util.Random
import scala.io.Source

class InsertionSortTest extends FlatSpec with Timeouts {

  lazy val SPEED_FAIL = 100 millis

  it should "do insertion sort!" in {

    var arr: Array[Int] = Array.fill(100)(Random.nextInt)
    Sort.insertionSort(arr)
    assert(isSorted(arr))

    arr = Array.fill(1)(Random.nextInt)
    Sort.insertionSort(arr)
    assert(isSorted(arr))

    arr = Array[Int]()
    Sort.insertionSort(arr)
    assert(isSorted(arr))
  }

  it should "catch a timeout exception because insertion sort is slow!" in {

    val arr: Array[Int] = loadArray("arr10000.csv")
    intercept[TestFailedDueToTimeoutException] {
      failAfter(SPEED_FAIL) {
        Sort.insertionSort(arr)
      }
    }
  }
}
