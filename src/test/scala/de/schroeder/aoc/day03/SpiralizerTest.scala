package de.schroeder.aoc.day03

import org.scalatest.FunSuite

class SpiralizerTest extends FunSuite {

  test("test example 1") {
    // given
    // when
    val steps = Spiralizer.steps(1)
    // then
    assert(steps == 0)
  }

  test("test example 2") {
    // given
    // when
    val steps = Spiralizer.steps(12)
    // then
    assert(steps == 3)
  }

  test("test example 3") {
    // given
    // when
    val steps = Spiralizer.steps(23)
    // then
    assert(steps == 2)
  }

  test("test example 4") {
    // given
    // when
    val steps = Spiralizer.steps(1024)
    // then
    assert(steps == 31)
  }

  test("test puzzle") {
    // given
    // when
    val steps = Spiralizer.steps(347991)
    // then
    assert(steps == 480)
  }

  test("test adjacent example") {
    // given
    // when
    val steps = Spiralizer.adjacentSum(768)
    // then
    assert(steps == 806)
  }

  test("test puzzle 2") {
    // given
    // when
    val steps = Spiralizer.adjacentSum(347991)
    // then
    assert(steps == 349975)
  }


}
