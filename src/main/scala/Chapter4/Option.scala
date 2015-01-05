package Chapter4

sealed trait Option[+A] {

   /**
    * map will convert the value inside the monad, or None if the input is None
    */
   def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(v) => Some(f(v))
   }

   /**
    * FlatMap will convert and flatten the value inside the monad to multiple values
    */
   def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

   /**
    * getOrElse will extract the monad value or provided a default value if the input is undefined
    */
   def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(v) => v
   }

   /**
    * orElse will return itself if defined or another lazily evaluated Option if None
    */
   def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)

   /**
    * Filter will return this if the predicate matches otherwise None
    */
   def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

   def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(oa => b.map(ob => f(oa, ob)))

   def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
      case Nil => Some(Nil)
      case head :: tail => head.flatMap(hh => sequence(tail).map(uu => hh :: uu))
   }

   def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a.foldRight(Some(Nil): Option[List[B]])((a, b) => map2(f(a), b)(_ :: _))

   def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(b => b)


}
