package de.schroeder.aoc.day10

import scala.annotation.tailrec

object Hasher {

  private def round(elements: Array[Int], inputLengths: List[Int], position: Int = 0, skipSize: Int = 0): (Int, Int, Array[Int]) = {

    def buildIndexes(currentPosition: Int, length: Int): List[Int] = {
      if (length > 0) {
        (currentPosition % elements.length) :: buildIndexes(currentPosition + 1, length - 1)
      } else {
        Nil
      }
    }

    @tailrec
    def execute(lengths: List[Int], currentPosition: Int, skipSize: Int, elements: Array[Int]): (Int, Int, Array[Int]) = {
      lengths match {
        case length :: tail =>
          val indexes = buildIndexes(currentPosition, length)
          val values = indexes.map(elements(_)).reverse
          indexes.zip(values).foreach { case (index, value) => elements.update(index, value)}
          execute(tail, currentPosition + length + skipSize, skipSize + 1, elements)
        case _ => (currentPosition, skipSize, elements)
      }

    }

    execute(
      elements = elements,
      currentPosition = position,
      skipSize = skipSize,
      lengths = inputLengths
    )
  }

  def productOfFirstTwoDigitsAfterOneRound(numberOfElements: Int, inputLengths: List[Int]): Int = {
    val (_, _, elements) = round(List.range(0, numberOfElements).toArray, inputLengths)
    elements(0) * elements(1)
  }

  def knotHash(input: String): String = {

    @tailrec
    def computeSparseHash(currentRound: Int, elements: Array[Int], position: Int, skipSize: Int, lengths: List[Int]): List[Int] = {
      if (currentRound > 0) {
        val (currentPosition, currentSkipSize, currentElements) = round(elements, lengths, position, skipSize)
        computeSparseHash(currentRound - 1, currentElements, currentPosition, currentSkipSize, lengths)
      } else {
        elements.toList
      }
    }

    def blocks(sparseHash: List[Int]): List[List[Int]] = {
      sparseHash match {
        case Nil =>
          Nil
        case _ =>
          sparseHash.take(16) :: blocks(sparseHash.drop(16))
      }
    }

    val sparseHash = computeSparseHash(
      currentRound = 64,
      elements = List.range(0, 256).toArray,
      position = 0,
      skipSize = 0,
      lengths = input.trim.map(_.toInt).toList ++ List(17, 31, 73, 47, 23)
    )

    val denseHash = blocks(sparseHash).map(block => block.reduce(_ ^ _))

    denseHash
      .map(_.toHexString)
      .map(hexString => if (hexString.length == 1) s"0$hexString" else hexString)
      .mkString
  }

}
