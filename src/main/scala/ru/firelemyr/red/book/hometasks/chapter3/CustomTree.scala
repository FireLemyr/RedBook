package ru.firelemyr.red.book.hometasks.chapter3

trait CustomTree {
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

}
