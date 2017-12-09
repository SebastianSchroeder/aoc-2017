package de.schroeder.aoc.day09

import de.schroeder.aoc.Files
import org.scalatest.FunSuite

class StreamerTest extends FunSuite {

  test("test empty group") {
    // given
    val group = "{}"
    // when
    val groupScore = Streamer.computeGroupScores(group)
    // then
    assert(groupScore._1 == 1)
    assert(groupScore._2 == 0)
  }

  test("test group containing group containing group") {
    // given
    val group = "{{{}}}"
    // when
    val groupScore = Streamer.computeGroupScores(group)
    // then
    assert(groupScore._1 == 6)
    assert(groupScore._2 == 0)
  }

  test("test group containing two groups") {
    // given
    val group = "{{},{}}"
    // when
    val groupScore = Streamer.computeGroupScores(group)
    // then
    assert(groupScore._1 == 5)
    assert(groupScore._2 == 0)
  }

  test("test group containing group containing three groups, one of which with another group") {
    // given
    val group = "{{{},{},{{}}}}"
    // when
    val groupScore = Streamer.computeGroupScores(group)
    // then
    assert(groupScore._1 == 16)
    assert(groupScore._2 == 0)
  }

  test("test group only containing garbage") {
    // given
    val group = "{<a>,<a>,<a>,<a>}"
    // when
    val groupScore = Streamer.computeGroupScores(group)
    // then
    assert(groupScore._1 == 1)
    assert(groupScore._2 == 4)
  }

  test("test group containing 4 groups all containing garbage") {
    // given
    val group = "{{<ab>},{<ab>},{<ab>},{<ab>}}"
    // when
    val groupScore = Streamer.computeGroupScores(group)
    // then
    assert(groupScore._1 == 9)
    assert(groupScore._2 == 8)
  }

  test("test group containing 4 groups all containing garbage with cancel character") {
    // given
    val group = "{{<!!>},{<!!>},{<!!>},{<!!>}}"
    // when
    val groupScore = Streamer.computeGroupScores(group)
    // then
    assert(groupScore._1 == 9)
    assert(groupScore._2 == 0)
  }

  test("test group containing another group with only garbage and cancel characters") {
    // given
    val group = "{{<a!>},{<a!>},{<a!>},{<ab>}}"
    // when
    val groupScore = Streamer.computeGroupScores(group)
    // then
    assert(groupScore._1 == 3)
    assert(groupScore._2 == 17)
  }

  test("test puzzle") {
    // given
    val group = Files.lines("/input-day09.txt").head
    // when
    val groupScore = Streamer.computeGroupScores(group)
    // then
    assert(groupScore._1 == 16827)
    assert(groupScore._2 == 7298)
  }

}
