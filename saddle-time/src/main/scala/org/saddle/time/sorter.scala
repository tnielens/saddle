package org.saddle.time

import org.joda.time._

import org.saddle.array.Sorter
import org.saddle.{ORD, array}
import org.saddle.scalar.ScalarTagTime
import org.saddle.array.PermuteMergeSort
import cats.kernel.Order

object timeSorter extends Sorter[DateTime] {
  def argSorted(arr: Array[DateTime])(implicit ord: ORD[DateTime]) = {
    val res = array.range(0, arr.length)
    implicit val longOrd: Order[Long] =
      Order.by(long => new DateTime(long, DateTimeZone.UTC))
    PermuteMergeSort.sort(ScalarTagTime.time2LongArray(arr), res)
    res
  }

  def sorted(arr: Array[DateTime])(implicit ord: ORD[DateTime]) = {
    array.take(arr, argSorted(arr), ScalarTagTime.missing)
  }
}
