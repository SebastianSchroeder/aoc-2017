package de.schroeder.aoc.day01

import scala.annotation.tailrec

object Summarizer {

  def sum(numbers: List[Int], lookAhead: Int): Int = {
    @tailrec
    def buildSum(nums: List[Int], remaining: Int, accu: Int): Int = {
      if (remaining > 0) {
        buildSum(nums.tail, remaining - 1, if (nums.head == nums.drop(lookAhead).head) nums.head + accu else accu)
      } else {
        accu
      }
    }
    buildSum(nums = numbers ++ numbers, remaining = numbers.size, accu = 0)
  }

}