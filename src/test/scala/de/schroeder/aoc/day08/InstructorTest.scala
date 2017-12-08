package de.schroeder.aoc.day08

import de.schroeder.aoc.Files
import org.scalatest.FunSuite

class InstructorTest extends FunSuite {

  test("parsing line") {
    // given
    val input = "b inc 5 if a > 1"
    // when
    val instruction = Instruction.parse(input)
    // then
    assert(instruction == Instruction(Register("b"), Operation(Inc, 5), Expression(Register("a"), GT, 1)))
  }

  test("parsing line with negative value") {
    // given
    val input = "c inc -20 if c == 10"
    // when
    val instruction = Instruction.parse(input)
    // then
    assert(instruction == Instruction(Register("c"), Operation(Inc, -20), Expression(Register("c"), EQ, 10)))
  }

  test("test example") {
    // given
    val instructions = List(
      "b inc 5 if a > 1",
      "a inc 1 if b < 5",
      "c dec -10 if a >= 1",
      "c inc -20 if c == 10")
      .map(Instruction.parse)
    // when
    val maxRegisterValue = Instructor.run(instructions)
    // then
    assert(maxRegisterValue == (1, 10))
  }

  test("test puzzle") {
    // given
    val instructions = Files.lines("/input-day08.txt").map(Instruction.parse)
    // when
    val maxRegisterValue = Instructor.run(instructions)
    // then
    assert(maxRegisterValue == (4832, 5443))
  }

}
