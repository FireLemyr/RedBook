package ru.firelemyr.red.book.hometasks

object Exercise_2_4 {
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = { (a: A, b: B) => f(a)(b) }

  def main(args: Array[String]): Unit = {
    val checkLength: String => Int => Boolean = x => y => x.length == y
    val checkLengthUncurry: (String, Int) => Boolean = uncurry(checkLength)
    println(checkLengthUncurry("123", 3))
    println(checkLengthUncurry("13", 3))
  }
}
