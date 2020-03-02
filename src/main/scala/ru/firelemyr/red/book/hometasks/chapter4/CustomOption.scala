package ru.firelemyr.red.book.hometasks.chapter4

trait CustomOption {
  sealed trait COption[+A]  {
    def map[B](f: A => B): COption[B]
    def flatMap[B](f: A => COption[B]): COption[B]
    def getOrElse[B >: A](default: => B): B
    def orElse[B >: A](ob: => COption[B]): COption[B]
    def filter(f: A => Boolean): COption[A]
  }

  def lift[A,B](f: A => B): COption[A] => COption[B] = _ map f

  //4.1 Implement all of the preceding functions on Option. As you implement each function,
  //try to think about what it means and in what situations you’d use it. We’ll explore when
  //to use each of these functions next. Here are a few hints for solving this exercise:
  // It’s fine to use pattern matching, though you should be able to implement all
  //the functions besides map and getOrElse without resorting to pattern matching.
  // For map and flatMap, the type signature should be enough to determine the
  //implementation.
  // getOrElse returns the result inside the Some case of the Option, or if the Option
  //is None, returns the given default value.
  // orElse returns the first Option if it’s defined; otherwise, it returns the second
  //Option

  case class CSome[+A](get: A) extends COption[A] {
    override def map[B](f: A => B): COption[B] = CSome(f(get))

    override def flatMap[B](f: A => COption[B]): COption[B] = f(get)

    override def getOrElse[B >: A](default: => B): B = get

    override def orElse[B >: A](ob: => COption[B]): COption[B] = CSome(get)

    override def filter(f: A => Boolean): COption[A] = if (f(get)) CSome(get) else CNone

  }
  case object CNone extends COption[Nothing] {
    override def map[B](f: Nothing => B): COption[B] = CNone

    override def flatMap[B](f: Nothing => COption[B]): COption[B] = CNone

    override def getOrElse[B >: Nothing](default: => B): B = default

    override def orElse[B >: Nothing](ob: => COption[B]): COption[B] = getOrElse(ob)

    override def filter(f: Nothing => Boolean): COption[Nothing] = CNone
  }


}
