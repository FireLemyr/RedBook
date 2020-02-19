package ru.firelemyr.red.book.hometasks.chapter2

object Exercise_2_3 {
  def curry[A,B,C](f: (A, B) => C): A => B => C = { a: A =>  f(a, _) }

  def main(args: Array[String]): Unit = {
    val checkLength = (x:String, y:Int) => x.length == y
    val checkLengthCarry: String => Int => Boolean = curry(checkLength)
    println(checkLengthCarry("123")(3))
    println(checkLengthCarry("13")(3))
  }
}
