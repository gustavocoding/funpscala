package Chapter2

import Chapter2.Exercise2_1._
import Chapter2.Exercise2_2._
import Chapter2.Exercise2_3._
import Chapter2.Exercise2_4._
import Chapter2.Exercise2_5._
import org.scalatest.Matchers._
import org.scalatest._


class Chapter2Spec extends FlatSpec {

   "Exercise 2.1" should "be able to do Fibonacci using different algorithms" in {
      def calculateFibonacci(n: Int)(f: Int => Int): Seq[Int] = (1 to n) map f

      val algorithms = Seq(fibonacciV0 _, fibonacciV1 _, fibonacciV2 _)
      val maxVal = 12
      val results = algorithms.map(calculateFibonacci(maxVal))

      assert(results.forall(_ == Seq(1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144)))

   }

   "Exercise 2.2" should "be able to say if array is sorted based on custom ordered" in {
      val input1 = Array(5, 10, 20, 40, 80)
      val input2 = Array(10, 1, 2)

      assert(isSorted(input1, (a: Int, b: Int) => b == a * 2))
      assert(!isSorted(input2, (a: Int, b: Int) => a <= b))

      assert(isSorted2(input1, (a: Int, b: Int) => b == a * 2))
      assert(!isSorted2(input2, (a: Int, b: Int) => a <= b))
   }

   "Exercise 2.3" should "be able to curry" in {
      def sum(x: Int, y: Int) = x + y
      val curried = curry(sum)
      curried(2)(3) shouldBe 5
   }

   "Exercise 2.4" should "be able to uncurry" in {
      def sum(x: Int, y: Int) = x + y
      val sumRecycled = uncurry(curry(sum))

      sumRecycled(10, 3) shouldBe 13
   }

   "Exercise 2.5" should "be able to compose" in {
      def USDtoGBP(input: Double): Double = input * 0.6
      def GBPtoBRL(input: Double): Double = input * 4

      def USDtoBRL = compose(USDtoGBP, GBPtoBRL)

      USDtoBRL(5000) shouldBe 12000d
   }


}
