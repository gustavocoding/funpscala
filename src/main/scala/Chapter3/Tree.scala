package Chapter3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def treeSize[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + treeSize(l) + treeSize(r)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(n) => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(n) => Leaf(f(n))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(z) => f(z)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def treeSize2[A](t: Tree[A]): Int = fold(t)(a => 1)(1 + _ + _)

  def maximum2(tree: Tree[Int]): Int = fold(tree)(identity)(_ max _)

  def depth2[A](tree: Tree[A]): Int = fold(tree)(a => 1)((a, b) => 1 + (a max b))

  def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)((z: A) => Leaf(f(z)): Tree[B])(Branch(_, _))


}