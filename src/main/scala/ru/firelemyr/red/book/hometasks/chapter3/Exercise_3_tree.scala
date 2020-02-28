package ru.firelemyr.red.book.hometasks.chapter3

object Exercise_3_tree extends CustomTree {

  // 3.25 Write a function size that counts the number of nodes (leaves and branches) in a tree
  def length[A](tree: Tree[A]): Int = tree match {
    case x: Branch[A] => length(x.left) + length(x.right)
    case _: Leaf[A] => 1
  }

  // 3.26 Write a function maximum that returns the maximum element in a Tree[Int]. (Note:
  //In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x
  //and y.)

  def max(tree: Tree[Int]): Int = tree match {
    case x: Branch[Int] => max(x.left) max max(x.right)
    case x: Leaf[Int] => x.value
  }

  // 3.27 Write a function depth that returns the maximum path length from the root of a tree
  //to any leaf

  def depth[T](tree: Tree[T]): Int = {
    def step(stepTree: Tree[T], level: Int): Int = stepTree match {
      case x: Branch[T] => step(x.left, level + 1) max step(x.right, level + 1)
      case _: Leaf[T] => level
    }

    step(tree, 1)
  }

  // 3.28 Write a function map, analogous to the method of the same name on List,
  // that modifies each element in a tree with a given function

  def map[T, B](tree: Tree[T])(f: T => B): Tree[B] =
    tree match {
      case x: Branch[T] => Branch(map(x.left)(f), map(x.right)(f))
      case x: Leaf[T] => Leaf(f(x.value))
    }

  // 3.29 Generalize size, maximum, depth, and map, writing a new function fold that abstracts
  //over their similarities. Reimplement them in terms of this more general function. Can
  //you draw an analogy between this fold function and the left and right folds for List?

  def fold[A, B](tree: Tree[A])(leafF: A => B, branchF: (B, B) => B):B = {
    tree match {
      case x: Branch[A] =>
        val leftFold = fold(x.left)(leafF, branchF)
        val rightFold = fold(x.right)(leafF, branchF)
        branchF(leftFold, rightFold)
      case x: Leaf[A] => leafF(x.value)
    }
  }

  def length2[A](tree: Tree[A]): Int = {
    fold(tree)(_ => 1, (l:Int, r:Int) => l + r )
  }

  def depth2[T](tree: Tree[T]): Int = {
    fold(tree)(_ => 1, (l:Int, r:Int) => 1 + (l max r))
  }

  def map2[T, B](tree: Tree[T])(f: T => B): Tree[B] = {
    fold(tree)(x => Leaf(f(x)), (l:Tree[B], r:Tree[B]) => Branch(l,r))
  }

  // for print
  implicit class PrintTree[A](value: Tree[A]) {

    def print = {
      val branch = "[]"
      val placeForBranch = "    "

      def buldStep(step: Int) = "".padTo(step, placeForBranch).mkString("")

      def step(stepTree: Tree[A], level: Int): Unit =
        stepTree match {
          case x: Branch[A] =>
            step(x.left, level + 1)
            println(buldStep(level) + branch)
            step(x.right, level + 1)
          case x: Leaf[A] => println(buldStep(level) + x.value.toString)
        }

      step(value, 0)
    }
  }


  def main(args: Array[String]): Unit = {
    val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(23)), Branch(Leaf(3), Branch(Leaf(33), Leaf(4))))
    println("3.25")
    println(length(tree))
    println("3.26")
    println(max(tree))
    println("3.27")
    println(depth(tree))
    println("3.28")
    println(map(tree)(_ + "-number").print)
    println("3.29")
    println(length2(tree))
    println(depth2(tree))
    map2(tree)(_*2).print
  }
}
