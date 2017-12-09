package de.schroeder.aoc.day09

import scala.annotation.tailrec

object Streamer {

  def computeGroupScores(group: String): (Int, Int) = {

    def scanGroup(characters: List[Char], depth: Int, score: Int, garbage: Int): (Int, Int) = {
      characters match {
        case '{' :: tail =>
          scanGroup(tail, depth + 1, score, garbage)
        case '}' :: tail =>
          scanGroup(tail, depth - 1, score + depth, garbage)
        case '<' :: tail =>
          scanGarbage(tail, depth, score, garbage)
        case _ :: tail =>
          scanGroup(tail, depth, score, garbage)
        case Nil =>
          (score, garbage)
      }
    }

    @tailrec
    def scanGarbage(characters: List[Char], depth: Int, score: Int, garbage: Int): (Int, Int) = {
      characters match {
        case '!' :: _ :: tail =>
          scanGarbage(tail, depth, score, garbage)
        case '>' :: tail =>
          scanGroup(tail, depth, score, garbage)
        case _ :: tail =>
          scanGarbage(tail, depth, score, garbage + 1)
      }
    }

    scanGroup(characters = group.toList, depth = 0, score = 0, garbage = 0)
  }

}
