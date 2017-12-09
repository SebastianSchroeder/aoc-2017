package de.schroeder.aoc.day02

import scala.annotation.tailrec

object Checker {

  def checksum(rows: List[List[Int]]): Int = {
    rows.map(row => row.max - row.min).sum
  }

  def divisibleResultsSum(rows: List[List[Int]]): Int = {
    @tailrec
    def findDivisiblePartners(candidate: Int, numbers: List[Int]): Option[Int] = {
      numbers match {
        case head :: tail =>
          if (candidate > head) {
            if (candidate % head == 0) Option(candidate / head) else findDivisiblePartners(candidate, tail)
          } else {
            if (head % candidate == 0) Option(head / candidate) else findDivisiblePartners(candidate, tail)
          }
        case Nil =>
          None
      }
    }
    @tailrec
    def computeDivisiblePartnersResult(row: List[Int]): Int = {
      row match {
        case head :: tail =>
          findDivisiblePartners(head, tail) match {
            case Some(result) => result
            case None => computeDivisiblePartnersResult(tail)
          }
      }
    }
    rows.map(computeDivisiblePartnersResult).sum
  }

}
