package FFT

import math._
import scala.virtualization.lms._
import LMS.Trig


trait FFT extends Trig {

  case class Complex(re: Rep[Double], im: Rep[Double]) {
    def +(that: Complex) = Complex(this.re + that.re, this.im + that.im)
    def -(that: Complex) = Complex(this.re - that.re, this.im - that.im)
    def *(that: Complex) = Complex(this.re * that.re - this.im * that.im, this.re * that.im + this.im * that.re)
  }
  

  def omega(k: Int, N: Int): Complex = {
    val kth = -2.0 * k * math.Pi / N
    Complex(cos(kth), sin(kth))
  }

  def splitEvenOdd[T](xs: List[T]): (List[T], List[T]) = (xs: @unchecked) match {
    case e :: o :: xt =>
      val (es, os) = splitEvenOdd(xt)
      ((e :: es), (o :: os))
    case Nil => (Nil, Nil)
    // cases?
  } 


  def mergeEvenOdd[T](even: List[T], odd: List[T]): List[T] = ((even, odd): @unchecked) match {
    case (Nil, Nil) =>
      Nil
    case ((e :: es), (o :: os)) =>
      e :: (o :: mergeEvenOdd(es, os))
    // cases?
  }

  def fft(xs: List[Complex]): List[Complex] = xs match {
    case (x :: Nil) => xs
    case _ =>
      val N = xs.length // assume it's a power of two
      val (even0, odd0) = splitEvenOdd(xs)
      val (even1, odd1) = (fft(even0), fft(odd0))
      val (even2, odd2) = (even1 zip odd1 zipWithIndex) map {
        case ((x, y), k) =>
          val z = omega(k, N) * y
          (x + z, x - z)
      } unzip;
      even2 ::: odd2
  }

}