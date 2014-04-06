/**
 * Created by Swaneet on 05.04.2014.
 */
object PolyA3 {
  // take a run at this with scala> PolyA3.run
  def run() {
    println(" ========== Swaneet =========== ")
    val myPoly = Polynom(4, 14, (-2), 15)
    test(myPoly(3), 243)
    println( Polynom(2,1,0,-1,-1,0,1) + Polynom( (3,11) ) )
    println(Polynom(1) ° Polynom(1))

    println(" ========== Esser =========== ")

    val zero = Polynom( 0 )
    val one = Polynom( 0, 0, 0, 1 )
    val p1 = Polynom( 0, 0, 0, 1, 1 )
    val p2 = Polynom( 4, 3, 2, 1 )
    val p3 = Polynom( 3, 0, 5 )
    val p4 = Polynom( -1, 1, -10 )
    val p5 = Polynom( (10, 50) )  // care! This is swaped! Compare to Essers version

    test(zero, Polynom( 0 ))
    test(one, Polynom( 1 ))
    test( zero ° p2, Polynom(0))
    test( p2 ° zero, Polynom(1))
    test( p2 ° p3, Polynom( (108, 6), (567,4), (996,2), (586,0) ))
    test( p2 * p3, Polynom( (12,5), (9,4), (26,3), (18,2), (10,1), (5,0) ) )
    test( (p2 ° p3)(-1), 2257)
    test( (p2 * p3)(-1), -16)
    test( p1 * p2, Polynom( 4, 7, 5, 3, 1 ))
    test( p3*p4, Polynom( -3, 3, -35, 5, -50 ))
    test( p1+p2, Polynom( 4, 3, 3, 2 ) )
    test( (p1+p2)(1), 12)
    test( (p1°p2)(1), 11)
    test( (p2°p1)(1), 49)
    test( (p1*p2)(1), 20)
    test( (p2*p1)(1), 20)
    test( p5, Polynom( (10,50) ))
  }

  // helper function to display test results
  def test(a: Any, b: Any) = {
    val tpl = if (a == b)
      (" == ", "")
    else (" /= ", "\nExpected: " + a + "\nResult:" + b)
    println(a.toString() + tpl._1.toString() + b.toString() + tpl._2.toString())
  }
}

// utilities to calculate with Polynoms
// only integral coefficients and non-negative exponents are allowed.
class Polynom private(csAssoc: List[Pair[Int, Int]]) {
  // List((coefficient, exponent))

  // the index at which the element lies indicates that coefficients exponent
  // so Vector(7,1,4,2) means 7 + x + 4x^2 + 2x^3
  val cs: Vector[Int] = fromAssocList(csAssoc)

  def fromAssocList(as: List[Pair[Int, Int]]): Vector[Int] = {
    // calculate the highest exponent and therefore the length of the array/vector
    val maxC: Int = as.maxBy(_._2)._2 + 1 // the plus 1 is because the zero counts as a coefficient
    val poly: Array[Int] = new Array[Int](maxC) // the new keyword makes an Array (of zeroes) of maxC length, instead of a singleton Array
    // if multiple coefficients are assigned to an exponent,
    // then all of them are added together
    as.foreach((tpl) => tpl match {
      case (c, exp) => require(exp >= 0); poly(exp) += c // the coefficient at the position is added
    })
    poly.toVector
  }

  // calculate the value at x
  def apply(x: Int) = cs.foldRight(0)((c, accu) => accu * x + c)

  // Addition of two polynoms
  def plus(p: Polynom): Polynom = this + p
  def +(p: Polynom): Polynom = {
    // we can simply concatenate the lists, because the constructor adds multiple coefficients the same exponent
    val newCS = p.cs.zipWithIndex ++ cs.zipWithIndex
    Polynom.fromVector(newCS)
  }


  // Multiplication of two polynoms
  def mult(p: Polynom): Polynom = this * p
  def *(p: Polynom): Polynom = {
    // benefits of Haskell-Do-Notation
    val ls = for {
      (c1, exp1) <- cs.zipWithIndex
      (c2, exp2) <- p.cs.zipWithIndex
      (newC, newExp) = (c1 * c2, exp1 + exp2)
    } yield (newC, newExp)
    Polynom.fromVector(ls)
  }


  // Polynon composition
  // (g ° f) = (g after f) = ( x => g(f(x)) )
  def after(p: Polynom): Polynom = this ° p
  def °(p: Polynom): Polynom = {
    cs
      .zipWithIndex
      .map((tpl) => tpl match {
          case (c, exp) => Polynom(c) * (p ^ exp)
        })
      .fold(Polynom(0))(_ + _)
  }

  // calculates a Polynom to an non-negative integral power
  def ^(exp: Int): Polynom = {
    // using a simple non-logarithmic power function.
    require(exp >= 0)
    if (exp == 0) Polynom(1)
    else this * (this ^ (exp - 1))
  }

  override def equals(p:Any):Boolean = p.isInstanceOf[Polynom] && cs == p.asInstanceOf[Polynom].cs

  override def toString(): String = {
    def step(tpl: Pair[Int, Int]): String = {
      val c = tpl._1
      val exp = tpl._2
      val expStr = exp match {
        case 0 => ""
        case 1 => "x"
        /*case 2 => "x²"  // unreadable on screen
        case 3 => "x³" */
        case e => "x^" + e
      }
      c match {
        case 0 if exp == 0 => "0" // ommit a zero summand
        case 0 => "" // ommit a zero summand
        case 1 if exp == 0 => "1" // write "1" insteadt of "" for 1*x^0
        case 1 => expStr  // write "x^n" instead of "1x^n"
        case -1 => "-" + expStr  // write "-x^n" instead of "-1x^n"
        case c => c + expStr
      }
    }

    cs
      .zipWithIndex // add exponents
      .reverse // staert with highest exponent
      .map(x => step(x)) // make strings
      .filter(_ != "") // filter zero summands
      .mkString(" + ") // join with "+"

    // if scala collections functions were lazy by default,
    // this composition of functions would only need
    // a single traversal instead of iterating 5 times
  }
}


object Polynom {
  // the cFirst in both of these apply methods forces the caller to give atleast one argument.

  // the creator methode with tuples has its integers switched.
  // I ALWAYS use (coefficient, exponent) for tuple usage
  def apply(cFirst: Int, csRest: Int*) = {
    val cs = cFirst :: csRest.toList
    // we make a association List out of the given coefficients
    // because the most significant coefficient appears at front,
    // we have to reverse the list before assigning the exponent
    val withExponents = cs.reverse.zipWithIndex
    new Polynom(withExponents)
  }

  def apply(cFirst: Pair[Int, Int], cs: Pair[Int, Int]*) = new Polynom( (cFirst :: cs.toList).map(_.swap) )

  def fromList(cs: List[Pair[Int, Int]]): Polynom = new Polynom(cs)

  def fromVector(cs: Vector[Pair[Int, Int]]): Polynom = new Polynom(cs.toList)
}
