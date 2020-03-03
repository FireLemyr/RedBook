package ru.firelemyr.red.book.hometasks.chapter4

object Exercise_4_option extends CustomOption {

  implicit class SeqCustomOption[T](value: Seq[T]) {
    def headCOption: COption[T] = if (value.isEmpty) CNone else CSome(value.head)
  }

  // 4.2 Implement the variance function in terms of flatMap. If the mean of a sequence is m,
  //the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.
  //See the definition of variance on Wikipedia (http://mng.bz/0Qsr).

  def variance(xs: Seq[Double]): COption[Double] = {
    xs.headCOption.map { _ =>
      val avg = xs.sum / xs.size
      xs.map(x => math.pow(x - avg, 2)).sum
    }
  }

  // 4.3 Write a generic function map2 that combines two Option values using a binary function.
  // If either Option value is None, then the return value is too. Here is its signature:

  def map2[A, B, C](a: COption[A], b: COption[B])(f: (A, B) => C): COption[C] = {
    a.flatMap { aGet =>
      b.map { bGet =>
        f(aGet, bGet)
      }
    }
  }

  // 4.4 Write a function sequence that combines a list of Options into one Option containing
  //a list of all the Some values in the original list. If the original list contains None even
  //once, the result of the function should be None; otherwise the result should be Some
  //with a list of all the values. Here is its signature:3

  def sequence[A](a: List[COption[A]]): COption[List[A]] = {
    a.headCOption.flatMap { h =>
      a.tail.foldLeft(h.map(e => List(e)))((acc, elem) => map2(acc, elem)((x, y) => x :+ y))
    }
  }

  // 4.5 Implement this function. It’s straightforward to do using map and sequence, but try
  //for a more efficient implementation that only looks at the list once. In fact, implement sequence in terms of traverse

  def traverse[A, B](a: List[A])(f: A => COption[B]): COption[List[B]] = {
    a.headCOption.flatMap { h =>
      a.tail.foldLeft(f(h).map(e => List(e))) { (acc, elem) =>
        map2(acc, f(elem))((x, y) => x :+ y)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println("4.2")
    println(variance(Seq(2.0, 7, 1)))
    println(variance(Seq()))
    println("4.3")
    println(map2(CSome(5), CSome(6))(_ + _))
    println(map2(CNone, CSome(6))((x: Int, y: Int) => x + y))
    println(map2(CSome(5), CNone)((x: Int, y: Int) => x + y))
    println("4.4")
    println(sequence(List(CSome(1), CSome(2), CSome(3))))
    println(sequence(List(CSome(1), CNone, CSome(3))))
    println(sequence(List(CSome(1), CSome(2), CNone)))
    println(sequence(List()))
    println("4.5")
    println(traverse(List("1", "2", "3"))(x => Try{x.toInt}))
    println(traverse(List("1", "dd", "3"))(x => Try{x.toInt}))
  }

}
