package de.schroeder.aoc.day11

import scala.annotation.tailrec

object Hexer {

  @tailrec
  def numberOfStepsBack(steps: List[String],
                        initialCoordinate: (Int, Int) = (0,0),
                        maxDistance: Int = 0): (Int, Int) = {
    val (x, y) = initialCoordinate
    steps match {
      case "n" :: tail =>
        numberOfStepsBack(tail, (x, y+1), Math.max(distance(x, y+1), maxDistance))
      case "ne" :: tail =>
        numberOfStepsBack(tail, (x+1, y), Math.max(distance(x+1, y), maxDistance))
      case "se" :: tail =>
        numberOfStepsBack(tail, (x+1, y-1), Math.max(distance(x+1, y-1), maxDistance))
      case "s" :: tail =>
        numberOfStepsBack(tail, (x, y-1), Math.max(distance(x, y-1), maxDistance))
      case "sw" :: tail =>
        numberOfStepsBack(tail, (x-1, y), Math.max(distance(x-1, y), maxDistance))
      case "nw" :: tail =>
        numberOfStepsBack(tail, (x-1, y+1), Math.max(distance(x-1, y+1), maxDistance))
      case Nil =>
        (distance(x, y), maxDistance)

    }
  }

  def distance(x: Int, y: Int): Int = {
    (Math.abs(y) + Math.abs(y + x) + Math.abs(x)) / 2
  }

}
