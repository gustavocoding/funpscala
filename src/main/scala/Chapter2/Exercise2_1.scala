package Chapter2

import scala.annotation.tailrec

object Exercise2_1 {

   // imperative
   def fibonacciV0(n: Int): Int = {
      var ant = 0
      var prev = 1
      for (i <- 1 to n) {
         val res = ant + prev
         ant = prev
         prev = res
      }
      ant
   }

   // Not tail recursive
   def fibonacciV1(n: Int): Int = {
      n match {
         case 0 => 0
         case 1 => 1
         case _ => fibonacciV1(n - 1) + fibonacciV1(n - 2)
      }
   }

   // Tail recursive
   def fibonacciV2(n: Int): Int = {
      @tailrec
      def f(n: Int, prev: Int, acc: Int): Int = {
         n match {
            case 0 => acc
            case _ => f(n - 1, acc, acc + prev)
         }
      }

      f(n, 1, 0)
   }

}
