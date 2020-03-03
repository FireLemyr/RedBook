package ru.firelemyr.red.book.hometasks.chapter4

import ru.firelemyr.red.book.hometasks.chapter4.Exercise_4_option.map2

object Exercise_4_either extends CustomEither with PersonFunctions{

  //4.7 Implement sequence and traverse for Either. These should return the first error
  //that’s encountered, if there is one

  def sequence[E, A](es: List[CEither[E, A]]): CEither[E, List[A]] = {
    val start: CEither[E, List[A]] = CRight(List())
    es.foldLeft(start)((acc, elem) => elem.map2(acc)((e, a) => a :+ e))
  }

  def traverse[E, A, B](as: List[A])(f: A => CEither[E, B]): CEither[E, List[B]] = {
    val start: CEither[E, List[B]] = CRight(List())
    as.foldLeft(start)((acc, elem) => f(elem).map2(acc)((e, a) => a :+ e))
  }

  // 4.8 In this implementation, map2 is only able to report one error, even if both the name
  //and the age are invalid. What would you need to change in order to report both errors?
  //Would you change map2 or the signature of mkPerson? Or could you create a new data
  //type that captures this requirement better than Either does, with some additional
  //structure? How would orElse, traverse, and sequence behave differently for that
  //data type?

  sealed trait AggregationEither[+E, +A] {
    self =>

    def map[B](f: A => B): AggregationEither[E, B]

    def flatMap[EE >: E, B](f: A => AggregationEither[EE, B]): AggregationEither[EE, B]

    def orElse[EE >: E, B >: A](b: => AggregationEither[EE, B]): AggregationEither[EE, B]

    def map2[EE >: E, B, C](b: AggregationEither[EE, B])(f: (A, B) => C): AggregationEither[EE, C]
  }

  case class AggregationLeft[+E](values: List[E]) extends AggregationEither[E, Nothing] {

    override def map[B](f: Nothing => B): AggregationEither[E, B] = AggregationLeft(values)

    override def flatMap[EE >: E, B](f: Nothing => AggregationEither[EE, B]): AggregationEither[EE, B] = AggregationLeft(values)

    override def orElse[EE >: E, B >: Nothing](b: => AggregationEither[EE, B]): AggregationEither[EE, B] = b

    override def map2[EE >: E, B, C](b: AggregationEither[EE, B])(f: (Nothing, B) => C): AggregationEither[EE, C] = {
      b match {
        case x:AggregationLeft[E] => AggregationLeft(values ++ x.values)
        case _:AggregationRight[B] => AggregationLeft(values)
      }
    }
  }

  object AggregationLeft {
    def apply[E](value: E): AggregationLeft[E] = AggregationLeft(List(value))
  }

  case class AggregationRight[+A](value: A) extends AggregationEither[Nothing, A] {
    override def map[B](f: A => B): AggregationEither[Nothing, B] = AggregationRight(f(value))

    override def flatMap[EE >: Nothing, B](f: A => AggregationEither[EE, B]): AggregationEither[EE, B] = f(value)

    override def orElse[EE >: Nothing, B >: A](b: => AggregationEither[EE, B]): AggregationEither[EE, B] = AggregationRight(value)

    override def map2[EE >: Nothing, B, C](b: AggregationEither[EE, B])(f: (A, B) => C): AggregationEither[EE, C] = {
      b.flatMap(bGet => AggregationRight(f(value, bGet)))
    }
  }

  def mkName2(name: String): AggregationEither[String, Name] =
    if (name == "" || name == null) AggregationLeft("Name is empty.")
    else AggregationRight(new Name(name))
  def mkAge2(age: Int): AggregationEither[String, Age] =
    if (age < 0) AggregationLeft("Age is out of range.")
    else AggregationRight(new Age(age))
  def mkPerson2(name: String, age: Int): AggregationEither[String, Person] =
    mkName2(name).map2(mkAge2(age))(Person(_, _))

  def main(args: Array[String]): Unit = {
    println("4.6")
    val left: CEither[String, Int] = CLeft[String]("left")
    val left2: CEither[String, Int] = CLeft[String]("left2")
    val right: CEither[String, Int] = CRight[Int](5)
    val right2: CEither[String, Int] = CRight[Int](7)
    right.map(x => x * 2).print
    left.map(x => x * 2).print
    right.flatMap(x => CRight(x * 2)).print
    left.flatMap(x => CRight(x * 2)).print
    left.orElse(right).print
    left.orElse(left2).print
    left.map2(right)(_ + _).print
    left.map2(left2)(_ + _).print
    right.map2(right2)(_ + _).print
    println("4.7")
    sequence(List(right, right2)).print
    sequence(List(right, left)).print
    traverse(List("1","2","3"))(x => Try{x.toInt}).print
    traverse(List("1","2","d3"))(x => Try{x.toInt}).print
    println("4.8")
    println(mkPerson2("", -10))
  }
}
