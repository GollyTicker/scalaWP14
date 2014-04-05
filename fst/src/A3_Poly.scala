/**
 * Created by Swaneet on 05.04.2014.
 */
object PolyA3{
  def test(a: Any, b: Any) = {
    val tpl = if (a == b)
      (" == ", "")
    else (" /= ", "\nExpected: " + a + "\nResult:" + b)
    println(a.toString() + tpl._1.toString() + b.toString() + tpl._2.toString())
  }

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
  }


}

class Polynom private (csAssoc: List[Pair[Int,Int]] ) {
  // (coefficient, exponent)

  val cs: Vector[Int] = fromAssocList(csAssoc)

  def fromAssocList(as:List[Pair[Int,Int]]):Vector[Int] = {

    val maxC:Int = as.maxBy(_._2)._2 + 1 // the plus 1 is because the zero counts as a coefficient
    val poly:Array[Int] = new Array[Int](maxC) // the new keyword prevents a single element Array
    as.foreach( (tpl) => tpl match { case (c, exp) => require(exp >= 0); poly(exp) = c} )
    poly.toVector
  }


  def apply (x: Int) = cs.foldRight(0) ((c, accu) => accu * x + c)

  def + (p: Polynom):Polynom = {
    /*import scala.math._
    val maxC:Int = max(this.cs.length, p.cs.length)
    val newPoly = for {
      i <- (0 to maxC)
      newC = this.cs(i) + p.cs(i)
    } yield (newC, i)
    */
    def withFixedLengths(c1:Vector[Int], c2:Vector[Int]):Vector[Int] = c1.zip(c2).map(0)( (tpl) => tpl._1 + tpl._2 )
    def addTrailingZeros(cs:Vector[Int], remaining:Int) = cs.++:( (new Array(remaining)).toVector )

    val lenDiff = p.cs.length - cs.length

    val newCS:Vector[Int] = if (lenDiff < 0) {
        val cs2 = addTrailingZeros(p.cs, lenDiff)
        withFixedLengths(cs, cs2)
      }
        else {
          val csFixed = addTrailingZeros(cs, lenDiff)
          withFixedLengths(csFixed, p.cs)
        }
    new Polynom(newCS.zipWithIndex.toList)
  }
  def * (p: Polynom) = p
  def ° (p: Polynom) = p

  override def toString():String = {
    def step(str:String, tpl:Pair[Int,Int]):String =  tpl._1.toString() + "x^" + tpl._2.toString() + " " + str
    cs.zipWithIndex
      .foldLeft("")((a, b) => step(a, b)) // bad IDEA warning. Idea is very buggy...
  }

}


object Polynom {
  def apply (cFirst: Int, csRest: Int *) = {
    val cs = cFirst :: csRest.toList
    val withExponents = cs.reverse.zipWithIndex
    new Polynom(withExponents)
  }
  def apply (cFirst: Pair[Int, Int], cs: Pair[Int, Int]* ) = new Polynom (cFirst :: cs.toList)

}
