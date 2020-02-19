package ru.firelemyr.red.book.hometasks.chapter2

object Exercise_2_2 {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def step(num: Int): Boolean = {
      as.length match {
        case n if Set(0, 1).contains(n) => true
        case n if n == num => true
        case _ if ordered(as(num - 1), as(num)) => step(num + 1)
        case _ => false
      }
    }

    step(1)
  }

  def printResult[A](ordered: (A, A) => Boolean)(array: Array[A]): Unit ={
    println(array.mkString("[ ", ", ", " ]") + " sorted: " + isSorted(array, ordered))
  }

  def main(args: Array[String]): Unit = {
    val ordered: (Int, Int) => Boolean = _<_
    val sortedArray = Array(1, 2, 3, 4)
    val noSortedArray = Array(1, 3, 2, 4)
    val arrayWithOneElem = Array(1)
    val emptyArray:Array[Int] = Array()
    val printer = printResult(ordered) _
    printer(sortedArray)
    printer(noSortedArray)
    printer(arrayWithOneElem)
    printer(emptyArray)
  }
}
