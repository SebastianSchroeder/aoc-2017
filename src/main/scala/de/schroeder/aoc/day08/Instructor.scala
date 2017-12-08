package de.schroeder.aoc.day08

import scala.annotation.tailrec
import scala.util.matching.Regex

case class Instruction(register: Register,
                       operation: Operation,
                       expression: Expression)

object Instruction {

  val InstructionPattern: Regex = """(\w+) (inc|dec) (\-?\d+) if (\w+) ([!=<>]+) (\-?\d+)""".r

  def parse(string: String): Instruction = {
    string match {
      case InstructionPattern(register, operator, value, conditionRegister, condition, conditionValue) =>
        Instruction(
          Register(register),
          Operation(Operator.fromString(operator), value.toInt),
          Expression(Register(conditionRegister), Condition.fromString(condition), conditionValue.toInt))
    }
  }

}

case class Register(id: String)

case class Operation(operator: Operator,
                     value: Int)

sealed trait Operator {

  def eval(o1: Int, o2: Int): Int

}

case object Inc extends Operator {
  override def eval(o1: Int, o2: Int): Int = o1 + o2
}

case object Dec extends Operator {
  override def eval(o1: Int, o2: Int): Int = o1 - o2
}

object Operator {

  def fromString(operator: String): Operator = {
    operator match {
      case "dec" => Dec
      case "inc" => Inc
    }
  }

}

case class Expression(register: Register,
                      condition: Condition,
                      value: Int)

sealed trait Condition {

  def eval(o1: Int, o2: Int): Boolean

}

case object LT extends Condition {
  override def eval(o1: Int, o2: Int): Boolean = o1 < o2
}

case object GT extends Condition {
  override def eval(o1: Int, o2: Int): Boolean = o1 > o2
}

case object EQ extends Condition {
  override def eval(o1: Int, o2: Int): Boolean = o1 == o2
}

case object NEQ extends Condition {
  override def eval(o1: Int, o2: Int): Boolean = o1 != o2
}

case object LTE extends Condition {
  override def eval(o1: Int, o2: Int): Boolean = o1 <= o2
}

case object GTE extends Condition {
  override def eval(o1: Int, o2: Int): Boolean = o1 >= o2
}


object Condition {

  def fromString(conditionString: String): Condition = {
    conditionString match {
      case "<" => LT
      case ">" => GT
      case "==" => EQ
      case "!=" => NEQ
      case "<=" => LTE
      case ">=" => GTE
    }
  }

}

object Instructor {

  //b inc 5 if a > 1

  def run(instructions: List[Instruction]) = {
    @tailrec
    def compute(ins: List[Instruction], registerValues: Map[Register, Int], maxValue: Int): (Int, Int) = {
      ins match {
        case Instruction(register, Operation(op, value), Expression(exprRegister, exprCondition, exprValue)) :: rest =>
          val exprRegisterValue = registerValues.getOrElse(exprRegister, 0)
          if (exprCondition.eval(exprRegisterValue, exprValue)) {
            val registerValue = registerValues.getOrElse(register, 0)
            val newRegisterValue = op.eval(registerValue, value)
            compute(rest, registerValues + (register -> newRegisterValue), Math.max(newRegisterValue, maxValue))
          } else {
            compute(rest, registerValues, maxValue)
          }
        case Nil => (registerValues.values.max, maxValue)
      }
    }
    compute(instructions, Map.empty, 0)
  }

}
