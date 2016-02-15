package org.cmgreen210.algorithms.sorttest

import org.cmgreen210.algorithms._
import org.cmgreen210.algorithms.sorttest.SortTestUtils._
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.concurrent.Timeouts
import org.scalatest.exceptions.TestFailedDueToTimeoutException
import org.scalatest.time.SpanSugar._


class SpeedTest extends FlatSpec with Timeouts {

  lazy val SPEED_FAIL = 100 millis
  lazy val arr: Array[Int] = loadArray("arr10000.csv")

  private def run(algo: Sort): Unit = {
    failAfter(SPEED_FAIL) {
      algo.sort(arr)
    }
  }

  def fail(algo: Sort): Unit = {
    it should "timeout" in {
      intercept[TestFailedDueToTimeoutException] {
        run(algo)
      }
    }
  }
  def pass(algo: Sort): Unit = {
    it should "pass" in {
      run(algo)
    }
  }
}

class InsertionSortTest extends FlatSpec with Sort with Timeouts with Matchers {

  override def sort[T: Ordering](arr: Array[T]): Array[T] ={
    InsertionSort.sort(arr)
  }

  it should "do insertion sort!" in {
    generalSortCheck(this)
  }

  it should "catch a timeout exception because insertion sort is slow!" in {
    val speedFail = new SpeedTest
    speedFail.fail(this)
  }

  it should "show stability" in {
    generalStabilityCheck(this)
  }
}

class SelectionSortTest extends FlatSpec with Sort with Timeouts with Matchers {

  override def sort[T: Ordering](arr: Array[T]): Array[T] ={
    SelectionSort.sort(arr)
  }

  it should "do insertion sort!" in {
    generalSortCheck(this)
  }

  it should "catch a timeout exception because insertion sort is slow!" in {
    val speedFail = new SpeedTest
    speedFail.fail(this)
  }

  it should "show instability" in {
    generalStabilityCheckFail(this)
  }
}
