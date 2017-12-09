package de.schroeder.aoc.day02

import de.schroeder.aoc.Files
import org.scalatest.FunSuite

class CheckerTest extends FunSuite {

  test("test parsing") {
    // given // when
    val rows = List("5 1 9 5", "7  5 3", "2  4 6 8").map(toIntLine)
    // then
    assert(rows == List(List(5, 1, 9, 5), List(7, 5, 3), List(2, 4, 6, 8)))
  }

  test("test example 1") {
    // given
    val rows = List("5 1 9 5", "7  5 3", "2  4 6 8").map(toIntLine)
    // when
    val checksum = Checker.checksum(rows)
    // then
    assert(checksum == 18)
  }

  test("test puzzle 1") {
    // given
    val rows = Files.lines("/input-day02.txt").map(toIntLine)
    // when
    val checksum = Checker.checksum(rows)
    // then
    assert(checksum == 41919)
  }

  test("test example 2") {
    // given
    val rows = List("5 9 2 8", "9 4 7 3", "3 8 6 5").map(toIntLine)
    // when
    val divisibleSum = Checker.divisibleResultsSum(rows)
    // then
    assert(divisibleSum == 9)
  }

  test("test puzzle 2") {
    // given
    val rows = Files.lines("/input-day02.txt").map(toIntLine)
    // when
    val divisibleSum = Checker.divisibleResultsSum(rows)
    // then
    assert(divisibleSum == 303)
  }


  private def toIntLine(line: String): List[Int] = {
    Files.splitLine(line).map(_.toInt)
  }


}
