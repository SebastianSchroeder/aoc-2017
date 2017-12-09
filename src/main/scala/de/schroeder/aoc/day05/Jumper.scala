package de.schroeder.aoc.day05

import scala.annotation.tailrec

object Jumper {

  // immutable data structures were a lot slower
  @tailrec
  def jump(instructionsToJump: Array[Int], currentPosition: Int, numberOfJumps: Int, createUpdateFunction: Int => (Int => Int)): Int = {
    val instruction = instructionsToJump(currentPosition)
    val newPosition = currentPosition + instruction
    if (newPosition >= 0 && newPosition < instructionsToJump.length) {
      val update: (Int) => Int = createUpdateFunction(instruction)
      instructionsToJump.update(currentPosition, update(instructionsToJump(currentPosition)))
      jump(instructionsToJump, newPosition, numberOfJumps + 1, createUpdateFunction)
    } else {
      numberOfJumps + 1
    }
  }

  def jumps(instructions: Array[Int]): Int = {
    jump(
      instructionsToJump = instructions,
      currentPosition = 0,
      numberOfJumps = 0,
      _ => _ + 1
    )
  }

  def strangerJumps(instructions: Array[Int]): Int = {
    jump(
      instructionsToJump = instructions,
      currentPosition = 0,
      numberOfJumps = 0,
      instruction => if (instruction >= 3) _ - 1 else _ + 1
    )
  }

}
