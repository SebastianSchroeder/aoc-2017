package de.schroeder.aoc.day04

import scala.annotation.tailrec

object Phraser {

  @tailrec
  def validate(phrase: List[String], condition: (String, List[String]) => Boolean, existingPhrases: List[String] = Nil): Int = {
    phrase match {
      case head :: tail =>
        if (condition(head, existingPhrases)) 0 else validate(tail, condition, head :: existingPhrases)
      case Nil =>
        1
    }
  }

  def validPhrases(phrases: List[List[String]]): Int = {
    val condition: (String, List[String]) => Boolean = (phrase, existingPhrases) => existingPhrases.contains(phrase)
    phrases.map(validate(_, condition)).sum
  }

  def advancedValidPhrases(phrases: List[List[String]]): Int = {
    val condition: (String, List[String]) => Boolean = (phrase, existingPhrases) => existingPhrases.exists(existingPhrase => existingPhrase.sorted == phrase.sorted)
    phrases.map(validate(_, condition)).sum
  }


}
