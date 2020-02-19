package ru.firelemyr.red.book.hometasks.chapter2

import scala.util.Try

object Exercise_2_1 {

  case class FibonachiCouple(prevNum: Int, curNum: Int) {
    def sum: Int = prevNum + curNum
  }

  def fibonachi(num: Int) = {
    @annotation.tailrec
    def step(lastCouple: FibonachiCouple, stepLevel: Int, maxLevel: Int): Int = {
      if (stepLevel + 1 == maxLevel) {
        lastCouple.curNum
      } else {
        val newCouple = FibonachiCouple(lastCouple.curNum, lastCouple.sum)
        step(newCouple, stepLevel + 1, maxLevel)
      }
    }

    num match {
      case n if n < 0 => throw new IllegalArgumentException("num cant be lover zero")
      case 0 => 0
      case _ => step(FibonachiCouple(0, 1), 0, num)
    }

  }

  def main(args: Array[String]): Unit = {
    // 0 1 1 2 3 5 8 13
    println(Try {
      fibonachi(-1)
    })
    println(fibonachi(0))
    println(fibonachi(1))
    println(fibonachi(5))
    println(fibonachi(7))

  }
}
