package Chapter4

import scala.math._

object Exercise4_2 {

  def mean(x: Seq[Double]): Double = x.foldLeft(0d)(_ + _) / x.length

  def variance(xs: Seq[Double]): Option[Double] = {
    val m = mean(xs)
    Some(mean(xs.map(d => pow(d - m, 2))))
  }

}
