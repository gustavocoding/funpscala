package Chapter3

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

   def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
   }

   def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
   }

   def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

   def tail[A](as: List[A]): List[A] = as match {
      case Nil => sys.error("tail of an empty list")
      case Cons(_, t) => t
   }

   def setHead[A](h: A, xs: List[A]) = xs match {
      case Nil => sys.error("cannot set head on empty list")
      case Cons(_, t) => Cons(h, t)
   }

   @tailrec
   def drop[A](n: Int, xs: List[A]): List[A] = (xs, n) match {
      case (l, 0) => l
      case (Nil, _) => Nil
      case (Cons(_, t), i) => drop(i - 1, t)
   }

   @tailrec
   def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
   }

   def init[A](l: List[A]): List[A] = l match {
      case Nil => sys.error("error")
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
   }

   def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
   }

   def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

   def length[A](as: List[A]): Int = foldRight(as, 0)((a, b) => b + 1)

   def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
      @tailrec
      def foldLeft0(acc: B, as: List[A]): B = as match {
         case Nil => acc
         case Cons(x, xs) => foldLeft0(f(acc, x), xs)
      }
      foldLeft0(z, as)
   }

   @tailrec
   def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
   }

   def append[A](l1: List[A], l2: List[A]): List[A] = l2 match {
      case Nil => l1
      case _ => foldRight(l1, l2)((a, b) => Cons(a, b))
   }

   def flatten[A](a: List[List[A]]): List[A] = foldLeft(a, List[A]())(append)

   def map[A, B](as: List[A])(f: A => B): List[B] = as match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
   }

   def sumFL[A, B >: A](ls: List[A])(implicit num: Numeric[A]): A = foldLeft(ls, num.zero)(num.plus)

   def productFL[A, B >: A](ds: List[A])(implicit num: Numeric[A]): A = foldLeft(ds, num.one)(num.times)

   def lengthFL[A](as: List[A]): Int = foldLeft(as, 0)((b, _) => b + 1)

   def reverse[A](xs: List[A]): List[A] = foldLeft(xs, List[A]())((a, b) => Cons(b, a))

   def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) Cons(h, filter(t)(f)) else filter(t)(f)
   }

   def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

   def filterUsingFlatmap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as) { a => if (f(a)) List(a) else List()}

   def zip[A](l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
      case (Nil, _) | (_, Nil) => List[Int]()
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zip(t1, t2))
   }

   def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = (l1, l2) match {
      case (Nil, _) | (_, Nil) => List[A]()
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
   }

   def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
      @tailrec
      def hasSub(l1: List[A], l2: List[A], isMatching: Boolean): Boolean = (l1, l2) match {
         case (Nil, Nil) => true
         case (Nil, _) => false
         case (_, Nil) => true
         case (Cons(h, t), Cons(h2, t2)) if h == h2 => hasSub(t, t2, isMatching = true)
         case (Cons(h, t), Cons(h2, t2)) if h != h2 => if (isMatching) false else hasSub(t, Cons(h2, t2), isMatching = false)
      }
      hasSub(sup, sub, isMatching = false)
   }

}
