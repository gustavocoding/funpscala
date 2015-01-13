package Chapter2

import scala.annotation.tailrec

object Exercise2_2 {

  // Using API
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    as.sliding(2).forall(a => ordered(a.head, a.tail.head))
  }

  //recursive
  def isSorted2[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def iter(length: Int, acc: Boolean): Boolean = {
      if (length == as.length - 1) acc
      else {
        iter(length + 1, acc && ordered(as(length), as(length + 1)))
      }
    }

    iter(0, true)

  }


}
