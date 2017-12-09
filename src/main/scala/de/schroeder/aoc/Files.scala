package de.schroeder.aoc

import scala.io.Source

object Files {

  def lines(path: String): List[String] = {
    Source.fromInputStream(Files.getClass.getResourceAsStream(path), "UTF-8").getLines().toList
  }

  def splitLine(line: String): List[String] = {
    line.split("\\s+").toList
  }

}
