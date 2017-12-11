package de.schroeder.aoc.day11

import de.schroeder.aoc.Files
import org.scalatest.FunSuite

class HexerTest extends FunSuite {

  test("test example 1") {
    // given
    val rows = List("ne,ne,ne").head.split(',').toList
    // when
    val steps = Hexer.numberOfStepsBack(rows)
    // then
    assert(steps._1 == 3)
  }

  test("test example 2") {
    // given
    val rows = List("ne,ne,sw,sw").head.split(',').toList
    // when
    val steps = Hexer.numberOfStepsBack(rows)
    // then
    assert(steps._1 == 0)
  }

  test("test example 3") {
    // given
    val rows = List("ne,ne,s,s").head.split(',').toList
    // when
    val steps = Hexer.numberOfStepsBack(rows)
    // then
    assert(steps._1 == 2)
  }

  test("test example 4") {
    // given
    val rows = List("se,sw,se,sw,sw").head.split(',').toList
    // when
    val steps = Hexer.numberOfStepsBack(rows)
    // then
    assert(steps._1 == 3)
  }

  test("test puzzle 1") {
    // given
    val rows = Files.lines("/input-day11.txt").head.split(',').toList
    // when
    val steps = Hexer.numberOfStepsBack(rows)
    // then
    assert(steps._1 == 761)
    assert(steps._2 == 1542)
  }

}
