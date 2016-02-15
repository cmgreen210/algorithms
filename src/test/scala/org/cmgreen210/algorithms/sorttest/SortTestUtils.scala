package org.cmgreen210.algorithms.sorttest

import org.cmgreen210.algorithms._
import scala.math.Ordered._
import scala.io.Source
import scala.util.Random

/**
 * Created by chris on 2/15/16.
 */
object SortTestUtils {
  def isSorted[T: Ordering](arr: Array[T]): Boolean = {
    for ( i <- 0 until (arr.length - 1)) {
      if ( arr(i) > arr(i + 1)) {
        return false
      }
    }
    true
  }

  def generalSortCheck(algo: Sort): Unit = {
    var arr: Array[Int] = Array.fill(100)(Random.nextInt)
    algo.sort(arr)
    assert(isSorted(arr))

    arr = Array.fill(1)(Random.nextInt)
    algo.sort(arr)
    assert(isSorted(arr))

    arr = Array[Int]()
    algo.sort(arr)
    assert(isSorted(arr))
  }

  def loadArray(path: String, sep: String = ","): Array[Int] = {
    Source.fromURL(getClass.getClassLoader.getResource(path).toString)
      .mkString.split(",").map(_.toInt)
  }

  class IntWrapper(intc: Int) extends Ordered[IntWrapper] {
    val int: Int = intc

    override def compare(that: IntWrapper): Int = this.int compare that.int
  }

  def getArrayForStableCheck: Array[IntWrapper] = {
    Array[IntWrapper](
      new IntWrapper(7),
      new IntWrapper(5),
      new IntWrapper(2),
      new IntWrapper(5)
    )
  }

  def generalStabilityCheck(algo: Sort): Unit = {
    val arr = getArrayForStableCheck
    val first5 = arr(1).toString
    val second5 = arr(3).toString
    algo.sort(arr)

    assert(arr(1).toString == first5)
    assert(arr(2).toString == second5)
  }
}

