package de.schroeder.aoc.day10

import de.schroeder.aoc.Files
import org.scalatest.FunSuite

class HasherTest extends FunSuite {

  def toInput(list: List[String]): List[Int] = {
    list.head.split("\\s*,\\s*").map(_.toInt).toList
  }

  test("test example 1") {
    // given
    val rows = toInput(List("3, 4, 1, 5"))
    // when
    val hash = Hasher.productOfFirstTwoDigitsAfterOneRound(5, rows)
    // then
    assert(hash == 12)
  }

  test("test puzzle 1") {
    // given
    val rows = toInput(Files.lines("/input-day10.txt"))
    // when
    val hash = Hasher.productOfFirstTwoDigitsAfterOneRound(256, rows)
    // then
    assert(hash == 6909)
  }

  test("test knot hash on empty string") {
    // given // when
    val hash = Hasher.knotHash("")
    // then
    assert(hash == "a2582a3a0e66e6e86e3812dcb672a272")
  }

  test("test knot hash on AoC 2017") {
    // given // when
    val hash = Hasher.knotHash("AoC 2017")
    // then
    assert(hash == "33efeb34ea91902bb2f59c9920caa6cd")
  }

  test("test knot hash on 1,2,3") {
    // given // when
    val hash = Hasher.knotHash("1,2,3")
    // then
    assert(hash == "3efbe78a8d82f29979031a4aa0b16a9d")
  }

  test("test knot hash on 1,2,4") {
    // given // when
    val hash = Hasher.knotHash("1,2,4")
    // then
    assert(hash == "63960835bcdc130f0b66d7ff4f6a5a8e")
  }

  test("test puzzle 2") {
    // given // when
    val hash = Hasher.knotHash(Files.lines("/input-day10.txt").head)
    // then
    assert(hash == "9d5f4561367d379cfbf04f8c471c0095")
  }

}
