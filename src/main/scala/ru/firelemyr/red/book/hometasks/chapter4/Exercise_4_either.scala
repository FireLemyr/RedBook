package ru.firelemyr.red.book.hometasks.chapter4

object Exercise_4_either extends CustomEither{

  def main(args: Array[String]): Unit = {
    println("4.6")
    val left: CEither[String, Int] = CLeft[String]("left")
    val left2: CEither[String, Int] = CLeft[String]("left2")
    val right: CEither[String, Int] = CRight[Int](5)
    val right2: CEither[String, Int] = CRight[Int](7)
    right.map(x => x*2).print
    left.map(x => x*2).print
    right.flatMap(x => CRight(x*2)).print
    left.flatMap(x => CRight(x*2)).print
    left.orElse(right).print
    left.orElse(left2).print
    left.map2(right)(_+_).print
    left.map2(left2)(_+_).print
    right.map2(right2)(_+_).print
  }
}
