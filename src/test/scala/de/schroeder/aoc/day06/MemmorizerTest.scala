package de.schroeder.aoc.day06

import de.schroeder.aoc.Files
import org.scalatest.FunSuite

class MemmorizerTest extends FunSuite {

  test("test example 1") {
    // given
    val rows = List("0", "2", "7", "0").map(_.toInt)
    // when
    val runs = Memmorizer.reallocate(rows)
    // then
    assert(runs._1 == 5)
    assert(runs._2 == 4)
  }

  test("test puzzle 1") {
    // given
    val rows = Files.lines("/input-day06.txt").flatMap(Files.splitLine).map(_.toInt)
    // when
    val runs = Memmorizer.reallocate(rows)
    // then
    assert(runs._1 == 5042)
    assert(runs._2 == 1086)
  }

}
