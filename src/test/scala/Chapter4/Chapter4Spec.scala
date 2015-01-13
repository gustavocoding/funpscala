package Chapter4

import Chapter4.Exercise4_2._
import Chapter4.Option._
import Chapter4.Validation._
import org.scalatest.Matchers._
import org.scalatest._

class Chapter4Spec extends FlatSpec {

  "Exercise 4.1" should "implement Option functions" in {
    val s1 = Some("value")
    val s2: Option[String] = None

    s1.map(_.length) shouldBe Some(5)
    s2.map(_.length) shouldBe None

    s1.getOrElse("fallback") shouldBe "value"
    s2.getOrElse("fallback") shouldBe "fallback"

    s1.flatMap((v: String) => Some(v.length)) shouldBe Some(5)
    s2.flatMap((v: String) => Some(v.length)) shouldBe None

    s1.orElse(Some("fallback")) shouldBe s1
    s2.orElse(Some("fallback")) shouldBe Some("fallback")

    s1.filter(_.length > 10) shouldBe None
    s1.filter(_.length > 1) shouldBe s1
  }

  "Exercise 4.2" should "implement variance" in {
    val s = Seq(1.0d, 2.0d, 6.0d)
    variance(s) shouldBe Some(4.666666666666667)
  }

  "Exercise 4.3" should "combine two options using a function" in {
    map2(Some("a"), Some("b"))((a, b) => a.concat(b)) shouldBe Some("ab")
    map2(Some("a"), Some("b"))((a, b) => a.concat(b).length) shouldBe Some(2)
    map2(Some("a"), None)((a, b) => a.concat(b).length) shouldBe None
  }

  "Exercise 4.4" should "convert a list of option into a single option" in {
    Option.sequence(List(Some("value1"), Some("value2"))) shouldBe Some(List("value1", "value2"))
    Option.sequence(List(Some("value1"), Some("value2"), None)) shouldBe None
    Option.sequence(List(None, Some("value2"))) shouldBe None
    Option.sequence(List(Some("value1"), None)) shouldBe None
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }

  "Exercise 4.5" should "apply a function that can fail to a list" in {
    val convert: String => Option[Int] = s => Try(s.toInt)
    Option.traverse(List("1", "2", "3"))(convert) shouldBe Some(List(1, 2, 3))
    Option.traverse(List("1", "2", "gustavo"))(convert) shouldBe None
  }

  "Exercise 4.6" should "do map, map2, flatMap, orElse and map on an Either data type" in {
    val aLeft: Either[String, Int] = Left("Runtime Exception")
    val default: Either[String, Int] = Right(2)
    val aRight: Either[String, List[Int]] = Right(List(1, 2, 3))
    // Left
    aLeft.map(a => 2) shouldBe aLeft
    aLeft.map2(default)((a, b) => b * b) shouldBe aLeft
    aLeft.flatMap(a => Left(2)) shouldBe aLeft
    aLeft.orElse(default) shouldBe default

    // Right
    aRight.map(_.size) shouldBe Right(3)
    aRight.flatMap(x => Right(x.toSet)) shouldBe Right(Set(1, 2, 3))
    aRight.flatMap(x => Left("OOME")) shouldBe Left("OOME")
    aRight.orElse(default) shouldBe aRight

    aRight.map2(Right(List(4, 5, 6)))(_ ::: _) shouldBe Right(List(1, 2, 3, 4, 5, 6))
    aRight.map2(Left("Error"))((a, b) => a) shouldBe Left("Error")
  }

  val unsafeFunction: String => Int = _.toInt
  val liftedUnsafeFunction: Either[Exception, String] => Either[Exception, Int] = Either.lift(unsafeFunction)
  val liftedUnsafeFunction2: String => Either[Exception, Int] = Either.liftAlt(unsafeFunction)


  "Exercise extra" should "lift function in the context of an Either" in {

    val aRight: Either[Exception, String] = Right("3")
    val aMaliciousRight: Either[Exception, String] = Right("nonNumber")

    val aLeft: Either[Exception, String] = Left(new Exception)

    liftedUnsafeFunction(aRight) shouldBe Right(3)
    liftedUnsafeFunction(aLeft) shouldBe aLeft
    liftedUnsafeFunction(aMaliciousRight).getClass shouldBe classOf[Left[Exception]]

  }

  "Exercise 4.7" should "sequence and traverse for Either type" in {
    case class TestException(message: String) extends java.lang.Exception

    Either.traverse(List("1", "2", "3", "4"))(liftedUnsafeFunction2) shouldBe Right(List(1, 2, 3, 4))
    Either.traverse(List("1", "2", "gustavo", "4"))(liftedUnsafeFunction2).getClass shouldBe classOf[Left[Exception]]
    Either.sequence(List(Right(1), Right(2), Right(3))) shouldBe Right(List(1, 2, 3))
    Either.sequence(List(Right(1), Left(new TestException("m")), Right(3))) shouldBe Left(new TestException("m"))
  }

  "Exercise 4.8" should "Either alternative to detect more than one error" in {
    mkPerson("", 30) shouldBe Left(List("Name is empty."))
    mkPerson("Gustavo", -1) shouldBe Left(List("Age is out of range."))
    mkPerson("", -1) shouldBe Left(List("Name is empty.", "Age is out of range."))

  }
}
