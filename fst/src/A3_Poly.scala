/**
 * Created by Swaneet on 05.04.2014.
 */
object PolyA3{
  // takea run at this
  def run() {
    val myPoly = Polynom(4,14,(-2),15)
    test(myPoly(3), 243)

    val zero= Polynom(0)
    val one= Polynom(0,0,0,1)
    val p1= Polynom(0,0,0,1,1)
    val p2= Polynom(4,3,2,1)
    val p3= Polynom(3,0,5)
    val p4= Polynom(-1,1,-10)
    val p5= Polynom((50,10))

    test( (p1 + p2)(1), 12 )
    test( (p2 * p3)(-1), -16)

    println(p2)
    println(p3)
    println(p2 * p3)
  }

  // helper function to display test results
  def test(a: Any, b: Any) = {
    val tpl = if (a == b)
      (" == ", "")
    else (" /= ", "\nExpected: " + a + "\nResult:" + b)
    println(a.toString() + tpl._1.toString() + b.toString() + tpl._2.toString())
  }
}

class Polynom private (csAssoc: List[Pair[Int,Int]] ) {
                          // List((coefficient, exponent))

  // the index at which the element lies indicates that coefficients exponent
  // so Vector(7,1,4,2) means 7 + x + 4x^2 + 2x^3
  val cs: Vector[Int] = fromAssocList(csAssoc)

  def fromAssocList(as:List[Pair[Int,Int]]):Vector[Int] = {
    val maxC:Int = as.maxBy(_._2)._2 + 1 // the plus 1 is because the zero counts as a coefficient
    val poly:Array[Int] = new Array[Int](maxC) // the new keyword makes an Array of tthis length, instead of a singleton Array

    // if multiple coefficients are assigned to an exponent,
    // then all of them are added together
    as.foreach( (tpl) => tpl match { case (c, exp) => require(exp >= 0); poly(exp) += c} )
    poly.toVector
  }

  // calculate the value at x
  def apply (x: Int) = cs.foldRight(0) ((c, accu) => accu * x + c)

  def addTrailingZeros(cs:Vector[Int], remaining:Int):Vector[Int] = cs.++( (new Array[Int](remaining)).toVector )

  // Addition of two polynoms
  def + (p: Polynom):Polynom = {
    def addPolynoms(c1:Vector[Int], c2:Vector[Int]):Vector[Int] = c1.zip(c2).map( (tpl) => tpl._1 + tpl._2 )
    // first we extends the smaller lists length with trailing zeros()
    // this then allows for addPolynoms() to safely traverse both lists
    val lenDiff = p.cs.length - cs.length
    val newCS = if (lenDiff < 0) {
        val cs2 = addTrailingZeros(p.cs, -lenDiff)
          addPolynoms(cs, cs2)
      }
        else {
          val csFixed = addTrailingZeros(cs, lenDiff)
          addPolynoms(csFixed, p.cs)
        }
    Polynom.fromVector(newCS.zipWithIndex)
  }

  // Multiplication of two polynoms
  def * (p: Polynom):Polynom = {  // lol, mult is so many times smaller than plus
    val ls = for {
      (c1, exp1) <- cs.zipWithIndex
      (c2, exp2) <- p.cs.zipWithIndex
      (newC, newExp) = (c1*c2, exp1+exp2) // benefits of Haskell-Do-Notation
    } yield (newC, newExp)
    Polynom.fromVector(ls)
  }

  // Polynon composition
  def ° (p: Polynom):Polynom = {
    for {
      (c, exp) <- p.cs.zipWithIndex  // using the second argument here, because function composition has flipped arguments
      partialPol = Polynom(a) * (p ^ exp)

    }
  }

  def ^ (exp:Int):Polynom {

  }

  override def toString():String = {
    cs
      .zipWithIndex
      .reverse
      .map( (tpl) => tpl._1.toString() + "x^" + tpl._2.toString()) // bad IDEA warning. Idea is very buggy...
      .mkString(" + ")
  }
}


object Polynom {
  // the cFirst in both of these apply methods forces the caller to give atleast one argument.
  def apply (cFirst: Int, csRest: Int *) = {
    val cs = cFirst :: csRest.toList
    // we make a association List out of the given coefficients
    // because the most significant coefficient appears at front,
    // we have to reverse the list before assigning the exponent
    val withExponents = cs.reverse.zipWithIndex
    new Polynom(withExponents)
  }
  def apply (cFirst: Pair[Int, Int], cs: Pair[Int, Int]* ) = new Polynom (cFirst :: cs.toList)

  def fromList(cs:List[Pair[Int,Int]]):Polynom = new Polynom(cs)
  def fromVector(cs:Vector[Pair[Int,Int]]):Polynom = new Polynom(cs.toList)



}
