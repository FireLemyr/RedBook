package ru.firelemyr.red.book.hometasks

object Exercise_2_5 {
  def compose[A, B, C](f: B => C, g: A => B): A => C = { a: A => f(g(a)) }

  def main(args: Array[String]): Unit = {
    val length: String => Int = x => x.length
    val messageBuilder: Int => String = x => s" String have size = $x"

    val printerLength: String => String = compose(messageBuilder, length)

    println(printerLength("Check function"))
  }
}
