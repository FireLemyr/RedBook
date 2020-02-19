package ru.firelemyr.red.book.hometasks.chapter3

object Exercise_3_1 extends CustomList{

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x //Возвращает 1й элемент если второй и 3й элементы 2 и 4 соответственно
    case Nil => 42 // Если объект это Nil то вернется 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y //Вернется сумма 1 и 2 элемента если 3й и 4й элементы равны 3 и 4 соответственно
    case Cons(h, t) => h + List.sum(t) // Для любых головы и хвоста вернет сумму
    case _ => 101 // Для всех остальных случаев
  }

}
