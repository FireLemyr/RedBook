package ru.firelemyr.red.book.hometasks.chapter3

object Exercise_3_1 {

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x,xs) => x + sum(xs)
    }
    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x,xs) => x * product(xs)
    }
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  def sum(a: List[Int]):Int  = ???

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x //Возвращает 1й элемент если второй и 3й элементы 2 и 4 соответственно
    case Nil => 42 // Если объект это Nil то вернется 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y //Вернется сумма 1 и 2 элемента если 3й и 4й элементы равны 3 и 4 соответственно
    case Cons(h, t) => h + sum(t) // Для любых головы и хвоста вернет сумму
    case _ => 101 // Для всех остальных случаев
  }
}
