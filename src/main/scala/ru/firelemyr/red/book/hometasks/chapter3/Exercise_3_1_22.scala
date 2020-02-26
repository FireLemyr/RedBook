package ru.firelemyr.red.book.hometasks.chapter3

object Exercise_3_1_22 extends CustomList {

  import List._

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x //Возвращает 1й элемент если второй и 3й элементы 2 и 4 соответственно
    case Nil => 42 // Если объект это Nil то вернется 42
    case Cons(
    x,
    Cons(
    y,
    Cons(3, Cons(4, _)))) => x + y //Вернется сумма 1 и 2 элемента если 3й и 4й элементы равны 3 и 4 соответственно
    case Cons(h, t) => h + List.sum(t) // Для любых головы и хвоста вернет сумму
    case _ => 101 // Для всех остальных случаев
  }


  //3.2 Implement the function tail for removing the first element of a List. Note that the
  //function takes constant time. What are different choices you could make in your
  //implementation if the List is Nil? We’ll return to this question in the next chapter

    def tail[T](ints: List[T]): List[T] = {
      ints match {
        case Cons(_, t) => t
        case _ => throw new IllegalArgumentException("tail is empty")
      }
    }

    //3.3 Using the same idea, implement the function setHead for replacing the first element
    //of a List with a different value.
    def setHead[T](ints: List[T], elem: T): List[T] = {
      ints match {
        case Cons(_, t) => Cons(elem, t)
        case _ => Cons(elem, Nil)
      }
    }

    //3.4 Generalize tail to the function drop, which removes the first n elements from a list.
    //Note that this function takes time proportional only to the number of elements being
    //dropped—we don’t need to make a copy of the entire List.

    def drop[A](l: List[A], n: Int): List[A] = {
      def step(as: List[A], level: Int): List[A] = {
        as match {
          case Nil => Nil
          case Cons(_, Nil) => Nil
          case Cons(_, t) if n > level => step(t, level + 1)
          case Cons(_, t) => t
        }
      }

      step(l, 1)
    }

    //3.5 Implement dropWhile, which removes elements from the List prefix as long as they
    //match a predicate
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
      def step(as: List[A]): List[A] = {
        as match {
          case Nil => Nil
          case Cons(h, t) if f(h) => step(t)
          case t@_ => t
        }
      }

      step(l)
    }

    //3.6 Not everything works out so nicely. Implement a function, init, that returns a List
    //consisting of all but the last element of a List. So, given List(1,2,3,4), init will
    //return List(1,2,3). Why can’t this function be implemented in constant time like
    //tail?

    def init[A](l: List[A]): List[A] = {
      def step(acc: List[A], balance: List[A]): List[A] = {
        balance match {
          case Cons(_, Nil) => acc
          case Cons(h, t) => step(List.append(acc, Cons(h, Nil)), t)
          case _ => Nil
        }
      }

      step(Nil, l)
    }


  //3.7 Can product, implemented using foldRight, immediately halt the recursion and
  //return 0.0 if it encounters a 0.0? Why or why not? Consider how any short-circuiting
  //might work if you call foldRight with a large list. This is a deeper question that we’ll
  //return to in chapter 5

  // Не может тк условие выхода из рекурсии - окончание списка.

  //3.9 Compute the length of a list using foldRight
  def length[T](ns: List[T]): Int =
    foldRight(ns, 0)((_, acc) => 1 + acc)

  //3.10 Our implementation of foldRight is not tail-recursive and will result in a StackOverflowError for large lists (we say it’s not stack-safe). Convince yourself that this is the
  //case, and then write another general list-recursion function, foldLeft, that is tail-recursive, using the techniques we discussed in the previous chapter. Here is its
  //signature:

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  //3.11 Write sum, product, and a function to compute the length of a list using foldLeft
  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x, y) => x + y)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def length3[T](ns: List[T]): Int =
    foldLeft(ns, 0)((acc, _) => 1 + acc)

  //3.12 Write a function that returns the reverse of a list (given List(1,2,3) it returns
  //List(3,2,1)). See if you can write it using a fold

  def reverse[T](ns: List[T]): List[T] = {
    foldLeft(ns, Nil: List[T])((acc, elem) => Cons(elem, acc))
  }

  // Hard: Can you write foldLeft in terms of foldRight? How about the other way
  //around? Implementing foldRight via foldLeft is useful because it lets us implement
  //foldRight tail-recursively, which means it works even for large lists without overflowing the stack.

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    val f2 = (x: B, y: A) => f(y, x)
    foldLeft(reverse(as), z)(f2)
  }

  def foldRight3[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(h, _))((func, elem) => (m: B) => func(f(elem, m)))(z)
    }
  }

  //3.14  Implement append in terms of either foldLeft or foldRight

  def append2[A](a1: List[A], a2: List[A]): List[A] = {
    foldLeft(a2, a1)((acc, elem) => Cons(elem, acc))
  }

  //3.15 Hard: Write a function that concatenates a list of lists into a single list. Its runtime
  //should be linear in the total length of all lists. Try to use functions we have already
  //defined
  // это думаю append2 тк foldLeft линеен

  // 3.16 Write a function that transforms a list of integers by adding 1 to each element.
  //(Reminder: this should be a pure function that returns a new List!)

  def addToElements(a1: List[Int], addCount: Int): List[Int] = {
    foldRight3(a1, Nil: List[Int])((elem, acc) => Cons(elem + addCount, acc))
  }

  // 3.17 Write a function that turns each value in a List[Double] into a String. You can use
  //the expression d.toString to convert some d: Double to a String

  def valueToString(a1: List[Double]): List[String] = {
    foldRight3(a1, Nil: List[String])((elem, acc) => Cons(elem.toString, acc))
  }

  // 3.18 Write a function map that generalizes modifying each element
  // in a list while maintaining the structure of the list. Here is its signature:12

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight3(as, Nil: List[B])((elem, acc) => Cons(f(elem), acc))
  }

  // 3.19 Write a function filter that removes elements from a list unless they satisfy a given
  //predicate. Use it to remove all odd numbers from a List[Int]

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    val maybeAddElem: (A, List[A]) => List[A] = (elem, acc) =>
      if (f(elem)) Cons(elem, acc) else acc
    foldRight3(as, Nil: List[A])(maybeAddElem)
  }

  // 3.20 Write a function flatMap that works like map except that the function given will return
  //a list instead of a single result, and that list should be inserted into the final resulting
  //list. Here is its signature:

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight3(as, Nil: List[B])((elem, acc) => append2(acc, f(elem)))
  }

  // 3.21 Use flatMap to implement filter.

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(x => if(f(x)) List(x) else Nil)
  }

  // 3.22 Write a function that accepts two lists and constructs a new list by
  // adding corresponding elements. For example, List(1,2,3) and List(4,5,6) become List(5,7,9)

  def head[A](a1:List[A]) = {
    a1 match {
      case Cons(h, _) => h
      case _ => throw new IllegalArgumentException("Nil have not head")
    }
  }

  def merge[A, B, C](a1:List[A], a2:List[B])(f: (A, B) => C): List[C] = {
    if(length3(a1) == length3(a2)) {
      foldRight3(a1, (Nil:List[C], reverse(a2))){ case (elem, (acc, balance)) =>
        (Cons(f(elem, head(balance)), acc), tail(balance))
      }._1
    } else {
      throw new IllegalArgumentException("Lists length not equal")
    }
  }

  // 3.23 Generalize the function you just wrote so that it’s not specific to integers or addition.
  //Name your generalized function zipWith

  def zipWith[A, B, C](a1:List[A], a2:List[B])(f: (A, B) => C): List[C] = merge(a1,a2)(f)


  // for print
  implicit class PrintList[A](value: List[A]) {
    def string: String = {
      val seq = foldRight3(value, Seq[A]())((elem, acc) => elem +: acc)
      seq.mkString("[ ", ", ", " ]")
    }
  }


  def main(args: Array[String]): Unit = {
    println("3.4")
    val list = List(1, 2, 3, 4, 5)
    println(drop(list, 3))
    println(drop(list, 7))
    println(drop(Nil, 3))
    println(drop(List(1), 1))
    println(drop(List(1), 2))
    println("3.5")
    println(dropWhile(list, (x: Int) => x < 4))
    println(dropWhile(list, (x: Int) => x < 40))
    println(dropWhile(list, (x: Int) => x < 0))
    println("3.6")
    println(init(list))
    println("3.8")
    println(List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))
    println("3.9")
    println(length(List(1, 2, 3)))
    println(length(List(1)))
    println(length(Nil))
    println("3.10")
    println(foldLeft(List(1, 2, 3), 4)(_ + _))
    println("3.11")
    println(sum3(list))
    println(product3(List(1.0, 2.0, 3.0)))
    println(length3(list))
    println("3.12")
    println(reverse(list))
    println("3.13")
    println(foldRight(List("a", "b", "c"), "0")(_ + _))
    println(foldRight2(List("a", "b", "c"), "0")(_ + _))
    println(foldRight3(List("a", "b", "c"), "0")(_ + _))
    println(foldLeft(List("a", "b", "c"), "0")(_ + _))
    println("3.14")
    println(append2(List(1, 2, 3), List(11, 22, 33)))
    println("3.16")
    println(addToElements(list, 1))
    println("3.17")
    println(valueToString(List(1.5, 6.7, 6.98)))
    println("3.18")
    println(map(list)((x: Int) => x + 1))
    println(map(List(1.5, 6.7, 6.98))(_.toString))
    println("3.19")
    println(filter(list)(_ % 2 == 0))
    println("3.20")
    println(flatMap(list)(i => List(i, i)).string)
    println("3.21")
    println(filter2(list)(_ % 2 == 0).string)
    println("3.22")
    println(merge(List(1,2,3), List(4,5,6))(_+_).string)
  }

}
