package de.schroeder.aoc.day05

import de.schroeder.aoc.Files
import org.scalatest.FunSuite

class JumperTest extends FunSuite {

  test("test example 1") {
    // given
    val rows = List("0", "3", "0", "1", "-3").map(_.toInt).toArray
    // when
    val checksum = Jumper.jumps(rows)
    // then
    assert(checksum == 5)
  }

  test("test puzzle 1") {
    // given
    val rows = Files.lines("/input-day05.txt").map(_.toInt).toArray
    // when
    val checksum = Jumper.jumps(rows)
    // then
    assert(checksum == 373543)
  }

  test("test example 2") {
    // given
    val rows = List("0", "3", "0", "1", "-3").map(_.toInt).toArray
    // when
    val checksum = Jumper.strangerJumps(rows)
    // then
    assert(checksum == 10)
  }

  test("test puzzle 2") {
    // given
    val rows = Files.lines("/input-day05.txt").map(_.toInt).toArray
    // when
    val checksum = Jumper.strangerJumps(rows)
    // then
    assert(checksum == 27502966)
  }

}
