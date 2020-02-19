package ru.firelemyr.red.book.hometasks.chapter3

object Exercise_3_2 extends CustomList {

  //3.2 Implement the function tail for removing the first element of a List. Note that the
  //function takes constant time. What are different choices you could make in your
  //implementation if the List is Nil? We’ll return to this question in the next chapter
  object ExpressionForList {
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
          case Cons(h, t) => step(List.append(acc,Cons(h, Nil)), t)
          case _=> Nil
        }

      }
      step(Nil, l)
    }
  }


  def main(args: Array[String]): Unit = {
    println("3.4")
    val list = List(1, 2, 3, 4, 5)
    println(ExpressionForList.drop(list, 3))
    println(ExpressionForList.drop(list, 7))
    println(ExpressionForList.drop(Nil, 3))
    println(ExpressionForList.drop(List(1), 1))
    println(ExpressionForList.drop(List(1), 2))
    println("3.5")
    println(ExpressionForList.dropWhile(list, (x: Int) => x < 4))
    println(ExpressionForList.dropWhile(list, (x: Int) => x < 40))
    println(ExpressionForList.dropWhile(list, (x: Int) => x < 0))
    println("3.6")
    println(ExpressionForList.init(list))
  }

}
