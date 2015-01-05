package Chapter4

object Validation {

   case class Person(name: Name, age: Age)

   sealed class Name(val value: String)

   sealed class Age(val value: Int)

   def mkName(name: String): Either[List[String], Name] =
      if (name == "" || name == null) Left(List("Name is empty."))
      else Right(new Name(name))

   def mkAge(age: Int): Either[List[String], Age] =
      if (age < 0) Left(List("Age is out of range."))
      else Right(new Age(age))

   def mkPerson(name: String, age: Int): Either[List[String], Person] = {
      val n: Either[List[String], Name] = mkName(name)
      val a: Either[List[String], Age] = mkAge(age)

      val errors = List(n, a).collect { case Left(err) => err}


      if (errors.nonEmpty) Left(errors.flatten) else n.map2(a)(Person)

   }

}
