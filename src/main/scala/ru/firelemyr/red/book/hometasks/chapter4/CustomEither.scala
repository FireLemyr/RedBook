package ru.firelemyr.red.book.hometasks.chapter4

trait CustomEither {
  sealed trait CEither[+E, +A]
  case class CLeft[+E](value: E) extends CEither[E, Nothing]
  case class CRight[+A](value: A) extends CEither[Nothing, A]
}
