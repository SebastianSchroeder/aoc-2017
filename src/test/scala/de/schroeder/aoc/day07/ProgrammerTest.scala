package de.schroeder.aoc.day07

import de.schroeder.aoc.Files
import org.scalatest.FunSuite

class ProgrammerTest extends FunSuite {

  test("parsing line with children") {
    // given
    val input = "cymmj (257) -> phyzvno, pmfprs, ozgprze, bgjngh"
    // when
    val disc = Disc.parse(input)
    // then
    assert(disc == Disc("cymmj", 257, List("phyzvno", "pmfprs", "ozgprze", "bgjngh")))
  }

  test("parsing line without children") {
    // given
    val input = "xhth (57)"
    // when
    val disc = Disc.parse(input)
    // then
    assert(disc == Disc("xhth", 57, Nil))
  }

  test("test example") {
    // given
    val discs = List("pbga (66)",
      "xhth (57)",
      "ebii (61)",
      "havc (66)",
      "ktlj (57)",
      "fwft (72) -> ktlj, cntj, xhth",
      "qoyq (66)",
      "padx (45) -> pbga, havc, qoyq",
      "tknk (41) -> ugml, padx, fwft",
      "jptl (61)",
      "ugml (68) -> gyxo, ebii, jptl",
      "gyxo (61)",
      "cntj (57)").map(Disc.parse)
    // when
    val bottomDisc = Programmer.buildProgram(discs)
    val imbalance = Programmer.weightImbalance(bottomDisc)
    // then
    assert(bottomDisc.name == "tknk")
    assert(imbalance == 60)
  }

  test("test puzzle") {
    // given
    val discs = Files.lines("/input-day07.txt").map(Disc.parse)
    // when
    val bottomDisc = Programmer.buildProgram(discs)
    // then
    assert(bottomDisc.name == "veboyvy")
  }

  test("test weight difference") {
    // given
    val discs = Files.lines("/input-day07.txt").map(Disc.parse)
    // when
    val imbalance = Programmer.weightImbalance(Programmer.buildProgram(discs))
    // then
    assert(imbalance == 749)
  }


}
