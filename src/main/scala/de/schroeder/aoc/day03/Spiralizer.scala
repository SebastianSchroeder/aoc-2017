package de.schroeder.aoc.day03

import scala.annotation.tailrec

object Spiralizer {

  def steps(grid: Int): Int = {

    @tailrec
    def determineNumberOfSpirals(spirals: Int, spiralMaxValue: Int): Int = {
      if (spiralMaxValue < grid) {
        val newSpiralsValue = spirals + 1
        determineNumberOfSpirals(newSpiralsValue, spiralMaxValue + newSpiralsValue * 8)
      } else {
        spirals
      }
    }

    @tailrec
    def determineDirectionValue(currentSpiral: Int = 0, directionIncrease: Int, value: Int = 1, numberOfSpirals: Int): Int = {
      if (currentSpiral < numberOfSpirals) {
        determineDirectionValue(currentSpiral + 1, directionIncrease + 8, value + directionIncrease, numberOfSpirals)
      } else {
        value
      }
    }

      val numberOfSpirals = determineNumberOfSpirals(spirals = 0, spiralMaxValue = 1)

      val eastDirectionValue = determineDirectionValue(directionIncrease = 1, numberOfSpirals = numberOfSpirals)
      val northDirectionValue = determineDirectionValue(directionIncrease = 3, numberOfSpirals = numberOfSpirals)
      val westDirectionValue = determineDirectionValue(directionIncrease = 5, numberOfSpirals = numberOfSpirals)
      val southDirectionValue = determineDirectionValue(directionIncrease = 7, numberOfSpirals = numberOfSpirals)

      val directionValues = List(eastDirectionValue, northDirectionValue, westDirectionValue, southDirectionValue)

      val minDistance = directionValues.map(grid - _).map(Math.abs).min

      minDistance + numberOfSpirals
  }

  def adjacentSum(grid: Int): Int = {

    def sumOfSurroundingValues(x: Int, y: Int, values: Map[(Int, Int), Int]): Int = {
      List((x+1, y), (x+1, y+1), (x, y+1), (x-1, y+1), (x-1, y), (x-1, y-1), (x, y-1), (x+1, y-1))
        .map(coordinate => values.getOrElse(coordinate, 0))
        .sum
    }

    def call(x: Int,
             y: Int,
             limit: Int,
             values: Map[(Int, Int), Int],
             function: (Int, Int, Int, Map[(Int, Int), Int]) => Int): Int = {
      val value = sumOfSurroundingValues(x, y, values)
      if (value > grid) {
        value
      } else {
        function(x, y, limit, values + ((x, y) -> value))
      }
    }

    def increase(x: Int, y: Int, max: Int, values: Map[(Int, Int), Int]): Int = {
      if (x < max) {
        call(x + 1, y, max, values, increase)
      } else if (y < max) {
        call(x, y + 1, max, values, increase)
      } else {
        decrease(x, y, -max, values)
      }
    }

    def decrease(x: Int, y: Int, min: Int, values: Map[(Int, Int), Int]): Int = {
      if (x > min) {
        call(x - 1, y, min, values, decrease)
      } else if (y > min) {
        call(x, y - 1, min, values, decrease)
      } else {
        increase(x, y, Math.abs(min) + 1, values)
      }
    }

    increase(0, 0, 1, Map((0, 0) -> 1))
  }

}
