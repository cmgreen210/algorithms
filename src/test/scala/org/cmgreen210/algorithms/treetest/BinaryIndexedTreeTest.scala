package org.cmgreen210.algorithms.treetest

import org.cmgreen210.algorithms.tree.BinaryIndexedTree
import org.cmgreen210.algorithms.sorttest.SortTestUtils.loadArray
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.concurrent.Timeouts
import org.scalatest.exceptions.TestFailedDueToTimeoutException
import org.scalatest.time.SpanSugar._

class BinaryIndexedTreeTest extends FlatSpec with Matchers
  with Timeouts {

  it should "do correct prefix sum" in {

    val elems = Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
    val tree = new BinaryIndexedTree(elems)

    for (i <- 1 until elems.length) {
      val s: Int = i * (i - 1) / 2
      tree.getPrefixSum(i - 1) should be (s)
    }
  }

  it should "do prefix sum quickly" in {
    val arr: Array[Int] = loadArray("arr10000.csv")
    val bit = new BinaryIndexedTree(arr)
    val idx: Int = 9999

    failAfter(1000 microseconds) {
      bit.getPrefixSum(idx)
    }
    intercept[TestFailedDueToTimeoutException] {
      failAfter(1000 microseconds) {
        var s: Int = 0
        for (i <- 0 until (idx - 1)) {
          s += arr(i)
        }
      }
    }
  }
}
