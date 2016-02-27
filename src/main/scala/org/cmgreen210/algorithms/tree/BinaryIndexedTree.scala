package org.cmgreen210.algorithms.tree

/**
 * Binary Indexed Tree
 */
class BinaryIndexedTree(seq: IndexedSeq[Int]) {
  private val l = seq.length
  private val arr = Array.fill[Int](l){0}

  for ((value, idx) <- seq.zipWithIndex) { update(value, idx)}

  def update(x: Int, value: Int): Unit = {
    var i = x + 1
    while (i <= l) {
      arr.update(i-1, arr(i-1) + value)
      i += i & -i
    }
  }

  def getPrefixSum(x: Int): Int = {
    assert(x >= 0 && x < l)
    var sum = 0
    var i = x + 1
    while (i > 0) {
      sum += arr(i-1)
      i -= i & -i
    }
    sum
  }
}
