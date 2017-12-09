package de.schroeder.aoc.day06

import scala.annotation.tailrec

object Memmorizer {

  def reallocate(memoryBanks: List[Int]): (Int, Int) = {

    def redistribute(memoryBanks: List[Int], memoryToDistribute: Int): (List[Int], Int) = {
      memoryBanks match {
        case head :: tail if memoryToDistribute > 0 =>
          val (banks, remainingMemory) = redistribute(tail, memoryToDistribute - 1)
          (head + 1 :: banks, remainingMemory)
        case _ =>
          (memoryBanks, memoryToDistribute)
      }
    }

    @tailrec
    def remainingRedistribution(memoryBanks: List[Int], memoryToDistribute: Int): List[Int] = {
      if (memoryToDistribute > 0) {
        val (redistributedMemoryBanks, remainingMemory) = redistribute(memoryBanks, memoryToDistribute)
        remainingRedistribution(redistributedMemoryBanks, remainingMemory)
      } else {
        memoryBanks
      }
    }

    def initialRedistribution(memoryBanks: List[Int], valueToFind: Int): (List[Int], Int) = {
      memoryBanks match {
        case head :: tail if head == valueToFind =>
          val (banks, remainingMemory) = redistribute(tail, head)
          (0 :: banks, remainingMemory)
        case head :: tail =>
          val (banks, remainingMemory) = initialRedistribution(tail, valueToFind)
          (head :: banks, remainingMemory)
      }
    }

    def redistributionLoop(memoryBanks: List[Int], previousConfigurations: List[(List[Int], Int)], numberOfRuns: Int): (Int, Int) = {
      val (initialRedistributedMemoryBanks, remainingMemory) = initialRedistribution(memoryBanks, memoryBanks.max)
      val completelyRedistributedMemoryBanks = remainingRedistribution(initialRedistributedMemoryBanks, remainingMemory)
      previousConfigurations.find(_._1 == completelyRedistributedMemoryBanks) match {
        case Some((_, run)) => (numberOfRuns, numberOfRuns - run)
        case None => redistributionLoop(completelyRedistributedMemoryBanks, (completelyRedistributedMemoryBanks, numberOfRuns) :: previousConfigurations, numberOfRuns + 1)
      }
    }
    redistributionLoop(memoryBanks, Nil, 1)
  }

}
