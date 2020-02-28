package ru.firelemyr.red.book.hometasks.chapter3

object Exercise_3_list {

  // 3.24 Hard: As an example, implement hasSubsequence for checking whether a List contains another List as a subsequence. For instance, List(1,2,3,4) would have
  //List(1,2), List(2,3), and List(4) as subsequences, among others. You may have
  //some difficulty finding a concise purely functional implementation that is also efficient. That’s okay. Implement the function however comes most naturally. We’ll
  //return to this implementation in chapter 5 and hopefully improve on it. Note: Any
  //two values x and y can be compared for equality in Scala using the expression x == y

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def step(list: List[A]): Boolean = {
      list match {
        case Nil => false
        case head :: _ => if (sub.head == head) checkSubSequence(list.tail, sub.tail) else step(list.tail)
      }
    }

    def checkSubSequence(sup: List[A], sub: List[A]): Boolean = {
      (sup, sub) match {
        case (head :: tail, head2 :: tail2) => if (head == head2) checkSubSequence(tail, tail2) else false
        case (_, Nil) => true
        case _ => false
      }
    }

    sub match {
      case Nil => true
      case _ :: _ => step(sup)
    }

  }

  def main(args: Array[String]): Unit = {
    val list = List(1, 2, 3, 4, 5)
    val subList = List(3, 4)
    val noSubList = List(7, 7)
    val emptyList = List()
    val list2 = List(4, 5, 7)
    val list3 = List(4)
    val list4 = List(4, 5)
    println(hasSubsequence(list, subList))
    println(hasSubsequence(list, noSubList))
    println(hasSubsequence(list, emptyList))
    println(hasSubsequence(list, list2))
    println(hasSubsequence(list, list3))
    println(hasSubsequence(list, list4))
    println(hasSubsequence(subList, list))
  }
}
