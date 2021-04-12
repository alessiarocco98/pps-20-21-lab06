package u06lab.code

import u06lab.code.Combiners.{CombinerConcat, CombinerMax, CombinerSum}

/**
  * 1) Implement trait Functions with an object FunctionsImpl such that the code
  * in TryFunctions works correctly.
 */

trait Functions {
  def sum(a: List[Double]): Double
  def concat(a: Seq[String]): String
  def max(a: List[Int]): Int // gives Int.MinValue if a is empty
  def combine[A](seq: Seq[A])(implicit combiner: Combiner[A]): A
}

object Combiners {
  implicit object CombinerSum extends Combiner[Double]{
    override def unit: Double = 0.0

    override def combine(a: Double, b: Double): Double = a + b
  }
  implicit object CombinerConcat extends Combiner[String] {
    override def unit: String = ""

    implicit override def combine(a: String, b: String): String = a + b
  }
  implicit object CombinerMax extends Combiner[Int] {
    override def unit: Int = Integer.MIN_VALUE

    implicit override def combine(a: Int, b: Int): Int = Integer.max(a,b)
  }
}

object FunctionsImpl extends Functions {
  /*
  override def sum(a: List[Double]): Double = {
    var sum: Double = 0
    a.foreach(e => sum = sum + e)
    sum
  }*/

  /*
  override def concat(a: Seq[String]): String = {
    var resultString = ""
    a.foreach(e => resultString = resultString + e)
    resultString
  }*/

  /*
  override def max(a: List[Int]): Int = {
    if(a.isEmpty){
      Integer.MIN_VALUE
    } else {
      a.max(Ordering[Int])
    }
  }*/

  override def sum(a: List[Double]): Double = combine(a)(implicitly[Combiner[Double]])
  override def concat(a: Seq[String]): String = combine(a)(implicitly[Combiner[String]])
  override def max(a: List[Int]): Int = combine(a)(implicitly[Combiner[Int]])

  def combine[A](seq: Seq[A])(implicit combiner: Combiner[A]): A = {
    var c:A = combiner.unit
    if (seq.nonEmpty) seq.foreach(e => c = combiner.combine(e,c) )
    c
  }
}
/*
  * 2) To apply DRY principle at the best,
  * note the three methods in Functions do something similar.
  * Use the following approach:
  * - find three implementations of Combiner that tell (for sum,concat and max) how
  *   to combine two elements, and what to return when the input list is empty
  * - implement in FunctionsImpl a single method combiner that, other than
  *   the collection of A, takes a Combiner as input
  * - implement the three methods by simply calling combiner
  *
  * When all works, note we completely avoided duplications..
 */
trait Combiner[A] {
  def unit: A
  def combine(a: A, b: A): A
}

object TryFunctionsComb extends App {
  val f: Functions = FunctionsImpl

  println(f.sum(List(10.0,20.0,30.1))) // 60.1
  println(f.sum(List()))                // 0.0
  println(f.concat(Seq("a","b","c")))   // abc
  println(f.concat(Seq()))              // ""
  println(f.max(List(-10,3,-5,0)))      // 3
  println(f.max(List()))                // -2147483648
}