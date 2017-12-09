package de.schroeder.aoc.day04

import de.schroeder.aoc.Files
import org.scalatest.FunSuite

class PhraserTest extends FunSuite {

  test("test example 1") {
    // given
    val rows = List("aa bb cc dd ee", "aa bb cc dd aa", "aa bb cc dd aaa").map(Files.splitLine)
    // when
    val validPhrases = Phraser.validPhrases(rows)
    // then
    assert(validPhrases == 2)
  }

  test("test puzzle 1") {
    // given
    val rows = Files.lines("/input-day04.txt").map(Files.splitLine)
    // when
    val validPhrases = Phraser.validPhrases(rows)
    // then
    assert(validPhrases == 386)
  }

  test("test example 2") {
    // given
    val rows = List("abcde fghij","abcde xyz ecdab", "a ab abc abd abf abj", "iiii oiii ooii oooi oooo", "oiii ioii iioi iiio")
      .map(Files.splitLine)
    // when
    val validPhrases = Phraser.advancedValidPhrases(rows)
    // then
    assert(validPhrases == 3)
  }

  test("test puzzle 2") {
    // given
    val rows = Files.lines("/input-day04.txt").map(Files.splitLine)
    // when
    val validPhrases = Phraser.advancedValidPhrases(rows)
    // then
    assert(validPhrases == 208)
  }

}
