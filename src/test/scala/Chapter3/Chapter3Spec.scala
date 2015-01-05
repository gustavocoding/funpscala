package Chapter3

import org.scalatest._
import org.scalatest.Matchers._
import Chapter3.List._
import Chapter3.Tree._


class Chapter3Spec extends FlatSpec {

   "Exercise 3.1" should "be able to detected correct value of expression" in {
      val res = List(1, 2, 3, 4, 5) match {
         case Cons(x, Cons(2, Cons(4, _))) => x
         case Nil => 42
         case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
         case Cons(h, t) => h + List.sum(t)
         case _ => 101
      }
      res shouldBe 3
   }

   "Exercise 3.2" should "be able to calculate tail" in {
      tail(List(1, 2, 3, 4, 5)) shouldBe List(2, 3, 4, 5)
      tail(List(1, 2)) shouldBe List(2)
      an[RuntimeException] should be thrownBy tail(List())
   }

   "Exercise 3.3" should "be able to replace head" in {
      setHead(4, List(1, 2, 3)) shouldBe List(4, 2, 3)
      an[RuntimeException] should be thrownBy setHead(1, List())
   }

   "Exercise 3.4" should "be able to drop elements from list" in {
      drop(0, List(1, 2, 3, 4, 5)) shouldBe List(1, 2, 3, 4, 5)
      drop(1, List(1, 2, 3, 4, 5)) shouldBe List(2, 3, 4, 5)
      drop(2, List(1, 2, 3, 4, 5)) shouldBe List(3, 4, 5)
      drop(3, List(1, 2, 3, 4, 5)) shouldBe List(4, 5)
      drop(4, List(1, 2, 3, 4, 5)) shouldBe List(5)
      drop(5, List(1, 2, 3, 4, 5)) shouldBe List()
      drop(5, List()) shouldBe List()
   }

   "Exercise 3.5" should "be able to do dropWhile" in {
      dropWhile(List(1, 2, 3), (x: Int) => x % 2 == 0) shouldBe List(1, 2, 3)
      dropWhile(List(2, 4, 6, 7), (x: Int) => x % 2 == 0) shouldBe List(7)
   }

   "Exercise 3.6" should "be able to get all but last elements" in {
      init(List(1, 2, 3)) shouldBe List(1, 2)
      init(List(2, 3)) shouldBe List(2)
      an[RuntimeException] should be thrownBy init(List())
   }

   "Exercise 3.8" should "see what happens when passing Nil and Cons foldRight" in {
      foldRight(List(1, 3), Nil: List[Int])(Cons(_, _)) shouldBe List(1, 3)
   }

   "Exercise 3.9" should "should calculate length using foldRight" in {
      List.length(List()) shouldBe 0
      List.length(List(1)) shouldBe 1
      List.length(List(1, 3)) shouldBe 2
      List.length(List(1, 2, 3)) shouldBe 3
   }

   "Exercise 3.10" should "should do a tailrec foldLeft" in {
      foldLeft(Nil: List[Int], 2)(_ + _) shouldBe 2
      foldLeft(List(1, 2, 3), 0)(_ + _) shouldBe 6
      foldLeft(List(1, 2, 3, 4), 0)((_, b) => b) shouldBe 4
   }

   "Exercise 3.11" should "should do sum, product and length in terms of foldleft" in {
      sumFL(List(1, 2, 3)) shouldBe 6
      sumFL(List(1.0, 2.0, 3.0)) shouldBe 6.0
      productFL(List(2, 3, 4)) shouldBe 24
      productFL(List(2, 0, 4)) shouldBe 0
      lengthFL(List()) shouldBe 0
      lengthFL(List(1)) shouldBe 1
      lengthFL(List(1, 3)) shouldBe 2
      lengthFL(List(1, 2, 3)) shouldBe 3
   }

   "Exercise 3.12" should "should reverse of a list using fold" in {
      reverse(List(1, 2, 3)) shouldBe List(3, 2, 1)
      reverse(List("a", "b", "c")) shouldBe List("c", "b", "a")
      reverse(List(1)) shouldBe List(1)
      reverse(Nil) shouldBe Nil
   }

   "Exercise 3.14" should "should append two lists using folds" in {
      append(List(1, 2, 3), List()) shouldBe List(1, 2, 3)
      append(List(), List()) shouldBe List()
      append(List(1), List(2)) shouldBe List(1, 2)
      append(List(1, 4, 5, 6), List(2, 1, 4)) shouldBe List(1, 4, 5, 6, 2, 1, 4)
   }

   "Exercise 3.15" should "should concat a list of list into a single list" in {
      flatten(List(List(), List(1, 2), List(3))) shouldBe List(1, 2, 3)
      flatten(List(List(1, 2, 3), List(1), List(10))) shouldBe List(1, 2, 3, 1, 10)
      flatten(List(List(1), List(2, 3, 4))) shouldBe List(1, 2, 3, 4)
   }

   "Exercise 3.16" should "should transform a list of integer by adding +1to each element" in {
      List.map(List[Int]())(_ + 1) shouldBe List()
      List.map(List[Int]())(_ + 1) shouldBe List()
      List.map(List(1, 4, 5, 6))(_ + 1) shouldBe List(2, 5, 6, 7)
      List.map(List(0))(_ + 1) shouldBe List(1)
   }

   "Exercise 3.17" should "should turn each elem of a List[Double] into String" in {
      List.map(List[Double]())(_.toString) shouldBe List()
      List.map(List(1.0, 2.0))(_.toString) shouldBe List("1.0", "2.0")
   }

   "Exercise 3.19" should "should filter list, removing all odds elements" in {
      filter(List(1, 2, 3, 4))(_ % 2 == 0) shouldBe List(2, 4)
      filter(List(1, 2, 3, 4))(_ > 4) shouldBe List()
   }

   "Exercise 3.20" should "should do flatmap" in {
      flatMap(List(1, 2, 3))(i => List(i, i)) shouldBe List(1, 1, 2, 2, 3, 3)
   }

   "Exercise 3.21" should "should do filter using flatMap" in {
      filterUsingFlatmap(List(1, 2, 3, 4))(_ % 2 == 0) shouldBe List(2, 4)
      filterUsingFlatmap(List(1, 2, 3, 4))(_ > 4) shouldBe List()
   }

   "Exercise 3.22" should "should sum correspondent elements of two lists" in {
      zip(List(1, 2, 3), List(4, 5, 6)) shouldBe List(5, 7, 9)
      zip(List(1), List(4, 5, 6)) shouldBe List(5)
   }

   "Exercise 3.23" should "should generalize zip function" in {
      zipWith(List(1), List(4, 5, 6))(_ + _) shouldBe List(5)
      zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) shouldBe List(5, 7, 9)
      zipWith(List("1", "2"), List("3", "4"))(_ + _) shouldBe List("13", "24")
   }

   "Exercise 3.24" should "should detect if one list is subsequence of another" in {
      hasSubsequence(List(1, 2, 3, 4), List(1)) shouldBe true
      hasSubsequence(List(1, 2, 3, 4), List(1, 2)) shouldBe true
      hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3)) shouldBe true
      hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3, 4)) shouldBe true
      hasSubsequence(List(1, 2, 3, 4), List(2, 3)) shouldBe true
      hasSubsequence(List(1, 2, 3, 4), List(2, 3, 4)) shouldBe true
      hasSubsequence(List(1, 2, 3, 4), List(1)) shouldBe true
      hasSubsequence(List(1, 2, 3, 4), List(2)) shouldBe true
      hasSubsequence(List(1, 2, 3, 4), List(3)) shouldBe true
      hasSubsequence(List(1, 2, 3, 4), List(4)) shouldBe true
      hasSubsequence(List(1, 2, 3, 4), List(4, 3)) shouldBe false
      hasSubsequence(List(1, 2, 3, 4), List(2, 1)) shouldBe false
      hasSubsequence(List(1, 2, 3, 4), List(3, 2, 1)) shouldBe false
      hasSubsequence(List(1, 2, 3, 4), List(3, 1, 2)) shouldBe false
      hasSubsequence(List(1, 2, 3, 4), List(5, 6)) shouldBe false
      hasSubsequence(List(1, 2, 3, 4), List(1, 2, 4)) shouldBe false
      hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3, 4, 5)) shouldBe false
      hasSubsequence(List(1, 2, 3, 4), List(1, 3, 5)) shouldBe false
   }

   "Exercise 3.25" should "should count nodes in a tree" in {
      treeSize(Leaf("String")) shouldBe 1
      treeSize(Branch(Leaf("A"), Leaf("B"))) shouldBe 3
      treeSize(Branch(Branch(Leaf("A"), Leaf("B")), Branch(Leaf("C"), Leaf("D")))) shouldBe 7
   }

   "Exercise 3.26" should "should find the maximum element of a tree" in {
      maximum(Leaf(1)) shouldBe 1
      maximum(Branch(Leaf(2), Leaf(3))) shouldBe 3
      maximum(Branch(Branch(Leaf(-1), Leaf(2)), Branch(Leaf(1), Leaf(0)))) shouldBe 2
   }

   "Exercise 3.27" should "should find the depth  of a tree" in {
      depth(Leaf(1)) shouldBe 1
      depth(Branch(Leaf(2), Leaf(3))) shouldBe 2
      depth(Branch(Branch(Leaf(-1), Leaf(2)), Branch(Leaf(1), Leaf(0)))) shouldBe 3
      depth(Branch(Leaf(-1), Branch(Leaf(1), Branch(Leaf(0), Branch(Leaf(1), Leaf(2)))))) shouldBe 5
   }

   "Exercise 3.28" should "map over a tree" in {
      Tree.map(Leaf(2))(_.toString) shouldBe Leaf("2")
      Tree.map(Leaf(2))(identity) shouldBe Leaf(2)
      Tree.map(Branch(Leaf(2), Leaf(3)))(_ + 1) shouldBe Branch(Leaf(3), Leaf(4))
      Tree.map(Branch(Branch(Leaf(-1), Leaf(2)), Branch(Leaf(1), Leaf(0))))(_ * 2) shouldBe Branch(Branch(Leaf(-2), Leaf(4)), Branch(Leaf(2), Leaf(0)))
   }

   "Exercise 3.29" should "do max, size, depth, map using fold" in {
      treeSize2(Leaf("String")) shouldBe 1
      treeSize2(Branch(Leaf("A"), Leaf("B"))) shouldBe 3
      treeSize2(Branch(Branch(Leaf("A"), Leaf("B")), Branch(Leaf("C"), Leaf("D")))) shouldBe 7

      maximum2(Leaf(1)) shouldBe 1
      maximum2(Branch(Leaf(2), Leaf(3))) shouldBe 3
      maximum2(Branch(Branch(Leaf(-1), Leaf(2)), Branch(Leaf(1), Leaf(0)))) shouldBe 2

      depth2(Leaf(1)) shouldBe 1
      depth2(Branch(Leaf(2), Leaf(3))) shouldBe 2
      depth2(Branch(Branch(Leaf(-1), Leaf(2)), Branch(Leaf(1), Leaf(0)))) shouldBe 3
      depth2(Branch(Leaf(-1), Branch(Leaf(1), Branch(Leaf(0), Branch(Leaf(1), Leaf(2)))))) shouldBe 5

      map2(Leaf(2))(_.toString) shouldBe Leaf("2")
      map2(Leaf(2))(identity) shouldBe Leaf(2)
      map2(Branch(Leaf(2), Leaf(3)))(_ + 1) shouldBe Branch(Leaf(3), Leaf(4))
      map2(Branch(Branch(Leaf(-1), Leaf(2)), Branch(Leaf(1), Leaf(0))))(_ * 2) shouldBe Branch(Branch(Leaf(-2), Leaf(4)), Branch(Leaf(2), Leaf(0)))

   }


}
