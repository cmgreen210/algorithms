package org.cmgreen210.algorithms.sorttest

import scala.math.Ordered._
import scala.io.Source

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

  def loadArray(path: String, sep: String = ","): Array[Int] = {
    Source.fromURL(getClass.getClassLoader.getResource(path).toString)
      .mkString.split(",").map(_.toInt)
  }
}
