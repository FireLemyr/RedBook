package ru.firelemyr.red.book.hometasks.chapter4

trait CustomEither {

  // 4.6 Implement versions of map, flatMap, orElse, and map2 on Either that operate on the
  //Right value.

  sealed trait CEither[+E, +A] {
    self =>
    def map[B](f: A => B): CEither[E, B]

    def flatMap[EE >: E, B](f: A => CEither[EE, B]): CEither[EE, B]

    def orElse[EE >: E, B >: A](b: => CEither[EE, B]): CEither[EE, B]

    def map2[EE >: E, B, C](b: CEither[EE, B])(f: (A, B) => C): CEither[EE, C]
    //-------------------------------
    def left: Option[E] = {
      self match {
        case _: CRight[A] => None
        case l: CLeft[E] => Some(l.value)
      }
    }

    def right: Option[A] = {
      self match {
        case r: CRight[A] => Some(r.value)
        case _: CLeft[E] => None
      }
    }

    def print: Unit = println(s"[ ${self.left}, ${self.right} ]")
  }

  case class CLeft[+E](value: E) extends CEither[E, Nothing] {
    override def map[B](f: Nothing => B): CEither[E, B] = CLeft(value)

    override def flatMap[EE >: E, B](f: Nothing => CEither[EE, B]): CEither[EE, B] = CLeft(value)

    override def orElse[EE >: E, B >: Nothing](b: => CEither[EE, B]): CEither[EE, B] = b

    override def map2[EE >: E, B, C](b: CEither[EE, B])(f: (Nothing, B) => C): CEither[EE, C] = CLeft(value)
  }

  case class CRight[+A](value: A) extends CEither[Nothing, A] {
    override def map[B](f: A => B): CEither[Nothing, B] = CRight(f(value))

    override def flatMap[EE >: Nothing, B](f: A => CEither[EE, B]): CEither[EE, B] = f(value)

    override def orElse[EE >: Nothing, B >: A](b: => CEither[EE, B]): CEither[EE, B] = CRight(value)

    override def map2[EE >: Nothing, B, C](b: CEither[EE, B])(f: (A, B) => C): CEither[EE, C] = {
      b.flatMap(bGet => CRight(f(value, bGet)))
    }
  }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }
}
