package ru.firelemyr.red.book.hometasks.chapter4

case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

trait PersonFunctions extends CustomEither{
  def mkName(name: String): CEither[String, Name] =
    if (name == "" || name == null) CLeft("Name is empty.")
    else CRight(new Name(name))
  def mkAge(age: Int): CEither[String, Age] =
    if (age < 0) CLeft("Age is out of range.")
    else CRight(new Age(age))
  def mkPerson(name: String, age: Int): CEither[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))
}
