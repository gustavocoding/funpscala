package Chapter4

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]

}

case class Left[+E](value: E) extends Either[E, Nothing] {
  override def map[B](f: (Nothing) => B): Either[E, B] = this

  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = this

  override def flatMap[EE >: E, B](f: (Nothing) => Either[EE, B]): Either[EE, B] = this

  override def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]): Either[EE, B] = b
}

case class Right[+A](value: A) extends Either[Nothing, A] {
  override def map[B](f: (A) => B): Either[Nothing, B] = Right(f(value))

  override def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = flatMap(a => b.map(c => f(a, c)))

  override def flatMap[EE >: Nothing, B](f: (A) => Either[EE, B]): Either[EE, B] = f(value)

  override def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]): Either[EE, B] = this
}

object Either {

  def tryc[E >: Exception, B](f: => B): Either[E, B] =
    try Right(f)
    catch {
      case e: Exception => Left(e)
    }

  def lift[A, B, E >: Exception](f: A => B): Either[E, A] => Either[E, B] = _ flatMap (x => tryc(f(x)))

  def liftAlt[A, B, E >: Exception](f: A => B): A => Either[E, B] = (a: A) => tryc(f(a))

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(identity)

  //   def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a.foldRight(Some(Nil): Option[List[B]])((a, b) => map2(f(a), b)(_ :: _))

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as.foldRight(Right(Nil): Either[E, List[B]])((a, b) => f(a).map2(b)(_ :: _))
}



