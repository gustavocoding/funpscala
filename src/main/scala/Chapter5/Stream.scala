package Chapter5

import scala.annotation.tailrec

sealed trait Stream[+A] {

   def toList: List[A]

   def take(n: Int): Stream[A]

   def drop(n: Int): Stream[A]

   def takeWhile(p: A => Boolean): Stream[A]

   def forAll(p: A => Boolean): Boolean

   def exists(p: A => Boolean): Boolean

   def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(hh, tt) => f(hh(), tt().foldRight(z)(f))
      case _ => z
   }

   def takeWhileFR(p: A => Boolean): Stream[A] = foldRight(Stream[A]())((a, b) => if (p(a)) Cons(() => a, () => b) else Empty)

   def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
   }

   def headOptionFR: Option[A] = foldRight(None: Option[A])((a, b) => Some(a))

   def map[B](f: A => B): Stream[B] = foldRight(Stream[B]())((a, b) => Cons(() => f(a), () => b))

   def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream[B]())((a, b) => f(a).append(b))

   def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((a, b) => Cons(() => a, () => b))

   def filter(p: A => Boolean): Stream[A] = foldRight(Stream[A]())((a, b) => if (p(a)) Cons(() => a, () => b) else b)
}


case object Empty extends Stream[Nothing] {
   override def toList: List[Nothing] = List()

   override def take(n: Int): Stream[Nothing] = this

   override def drop(n: Int): Stream[Nothing] = this

   override def takeWhile(p: (Nothing) => Boolean): Stream[Nothing] = this

   override def forAll(p: (Nothing) => Boolean): Boolean = false

   override def exists(p: (Nothing) => Boolean): Boolean = false

}

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
   override def toList: List[A] = h() :: t().toList

   override def take(n: Int): Stream[A] = this match {
      case Cons(hh, _) if n == 1 => Cons(hh, () => Stream.empty)
      case Cons(hh, tt) => Cons(hh, () => tt().take(n - 1))
   }

   override def drop(n: Int): Stream[A] = this match {
      case _ if n == 0 => this
      case Cons(hh, tt) => tt().drop(n - 1)
   }

   override def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(hh, tt) if p(hh()) => Cons(hh, () => tt().takeWhile(p))
      case _ => Empty
   }

   override def forAll(p: (A) => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

   override def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)
}

object Stream {
                               // f(4) = f(3) + f(2) =         f(1) +f(0) + f(1) +     f(1) + f(0)
   def fibsRecursive(): Stream[Int] = {
      def fibsInternal(n: Int, acc: Int): Int = n match {
         case 0 => 0
         case 1 => 1
         case d => fibsInternal(d - 1 + d -  2)
      }
      from(0) map fibsInternal
   }

   def fibs(): Stream[Int] = {
      @tailrec
      def fibsInternal(n1: Int, n2: Int, acc: Int)(f: (Int, Int) => Int): Int = if (acc > 0) fibsInternal(n2, f(n1, n2), acc - 1)(f) else n1

      from(0).map(fibsInternal(0, 1, _)((a, b) => a + b))
   }

   def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???

   def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
   }

   def empty[A]: Stream[A] = Empty

   def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

   def constant[A](a: A): Stream[A] = cons(a, constant(a))

   def from(n: Int): Stream[Int] = Cons(() => n, () => from(n + 1))
}