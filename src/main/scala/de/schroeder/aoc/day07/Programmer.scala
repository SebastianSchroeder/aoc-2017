package de.schroeder.aoc.day07

import scala.annotation.tailrec
import scala.util.matching.Regex

case class DiscTree(name: String, weight: Int, children: List[DiscTree]) {

  lazy val totalWeight: Int = {
    weight + children.map(_.totalWeight).sum
  }

}

case class Disc(name: String, weight: Int, children: List[String])

object Disc {

  val DiscWithChildrenPattern: Regex = """(\w+) \((\d+)\) -> (.+)""".r
  val DiscPattern: Regex = """(\w+) \((\d+)\)""".r

  def parse(string: String): Disc = {
    string match {
      case DiscWithChildrenPattern(name, weight, childNames) =>
        Disc(name, weight.toInt, childNames.split(",").map(_.trim).toList)
      case DiscPattern(name, weight) =>
        Disc(name, weight.toInt, Nil)
    }
  }

}

object Programmer {

  def buildProgram(discs: List[Disc]): DiscTree = {
    val discsWhoAreChildren = discs.flatMap(_.children).toSet
    val (childDiscs, rootDisc :: Nil) = discs.partition(disc => discsWhoAreChildren.contains(disc.name))
    val childDiscsByName = childDiscs.map(disc => disc.name -> disc).toMap

    def buildTree(children: List[String]): List[DiscTree] = {
      children
        .map(childDiscsByName(_))
        .map(disc => DiscTree(disc.name, disc.weight, buildTree(disc.children)))
    }

    DiscTree(rootDisc.name, rootDisc.weight, buildTree(rootDisc.children))
  }

  @tailrec
  def weightImbalance(discTree: DiscTree): Int = {
    val childrenByWeight = discTree.children.groupBy(_.totalWeight)
    val (nodeWithDifferentWeight, nodesWithCorrectWeight) = childrenByWeight.partition(_._2.size == 1)
    nodeWithDifferentWeight.headOption match {
      case Some((weight, node :: Nil)) =>
        if (node.children.groupBy(_.totalWeight).size > 1) { // the imbalance is coming from below
          weightImbalance(node)
        } else { // imbalance caused in this level, compute correct weight
          node.weight + (nodesWithCorrectWeight.keys.head - weight)
        }
      case _ => 0
    }

  }


}
