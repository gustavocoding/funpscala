package Chapter5

import org.scalatest.Matchers._

import org.scalatest.FlatSpec

class Chapter5Spec extends FlatSpec {

   "Exercise 5.1" should "convert a Stream to a List" in {
      Stream(1, 2, 3).toList shouldBe List(1, 2, 3)
      Stream.empty.toList shouldBe List()
   }

   "Exercise 5.2" should "implement take and drop" in {
      Stream("a", "b", "c").take(1).toList shouldBe List("a")
      Stream(1, 2, 3).take(1).toList shouldBe List(1)
      Stream(1, 2, 3, 4, 5, 6, 7).take(3).toList shouldBe List(1, 2, 3)

      lazy val ones: Stream[Int] = Stream.cons(1, ones)
      ones.take(2).toList shouldBe List(1, 1)

      val s: Stream[Int] = Stream.cons(4 / 2, Stream.cons(2 / 0, Stream.cons(1 / 0, Stream.cons(10 / 2, Stream.empty))))

      s.take(1).toList shouldBe List(2)

      Stream("a", "b", "c").drop(1).toList shouldBe List("b", "c")
      Stream("a", "b", "c").drop(2).toList shouldBe List("c")
      ones.drop(10).take(1).toList shouldBe List(1)

      s.drop(3).toList shouldBe List(5)

   }

   "Exercise 5.3" should "take from Stream while the predicate holds" in {
      Stream(2, 4, 6, 7).takeWhile(_ % 2 == 0).toList shouldBe List(2, 4, 6)

      val s: Stream[Int] = Stream.cons(10 / 2, Stream.cons(10 / 1, Stream.cons(10 / 0, Stream.empty)))

      s.takeWhile(_ > 4).take(2).toList shouldBe List(5, 10)
   }

   "Exercise 5.4" should "do a forAll in a lazy way" in {
      Stream(2, 4, 6, 7).forAll(_ % 2 == 0) shouldBe false

      val s: Stream[Int] = Stream.cons(2, Stream.cons(3, Stream.cons(4 / 0, Stream.empty)))

      s.forAll(_ < 3) shouldBe false

   }

   "Exercise 5.5" should "do a takeWhile using foldRight" in {
      Stream(2, 4, 6, 7).takeWhileFR(_ % 2 == 0).toList shouldBe List(2, 4, 6)

      val s: Stream[Int] = Stream.cons(10 / 2, Stream.cons(10 / 1, Stream.cons(10 / 0, Stream.empty)))

      s.takeWhileFR(_ > 4).take(2).toList shouldBe List(5, 10)

   }

   "Exercise 5.6" should "use foldRight to implement headOption" in {
      Stream(2, 4, 6, 7).headOptionFR shouldBe Some(2)
      Stream().headOptionFR shouldBe None

      val s: Stream[Int] = Stream.cons(10 / 2, Stream.cons(10 / 0, Stream.empty))

      s.headOptionFR shouldBe Some(5)
   }

   "Exercise 5.7" should "implement map, filter, flatMap and append using foldRight" in {
      val stream1: Stream[Int] = Stream(2, 4, 6, 7)
      val stream2: Stream[Int] = Stream.cons(2, Stream.cons(3 / 0, Stream.empty))

      stream1.map(_ * 2).take(2).toList shouldBe List(4, 8)
      stream2.map(_ + 1).take(1).toList shouldBe List(3)

      stream1.filter(_ % 2 == 0).take(3).toList shouldBe List(2, 4, 6)
      stream2.filter(_ < 10).take(1).toList shouldBe List(2)

      stream1.flatMap(Stream(1, _)).toList shouldBe List(1, 2, 1, 4, 1, 6, 1, 7)
      stream2.flatMap(Stream(_, 10)).take(2).toList shouldBe List(2, 10)

      stream1.append(stream2).take(5).toList shouldBe List(2, 4, 6, 7, 2)
   }

   "Exercise 5.8" should "generate an infinite stream" in {
      Stream.constant("A").take(2).toList shouldBe List("A", "A")
   }

   "Exercise 5.9" should "generate an infinite stream with +1 the prev elemetn" in {
      Stream.from(2).take(5).toList shouldBe List(2, 3, 4, 5, 6)
   }

}
