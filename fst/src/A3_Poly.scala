/**
 * Created by Swaneet on 05.04.2014.
 */
object PolyA3{
  def test(a: AnyRef, b: AnyRef) = {
    val tpl = if (a == b)
      (" == ", "")
    else (" /= ", "\nExpected: " + a + "\nResult:" + b)
    "a" + tpl._1 + "b" + tpl._2
  }

  def run() {
    val myPoly = Polynom(1,2,3,4)

    println("myPoly:" + myPoly)
    println("myPoly(3):" + myPoly(3))
  }


}

class Polynom private (csAssoc: List[Pair[Int,Int]] ) {

  val cs = fromAssocList(csAssoc)
  val len = cs.length

  def fromAssocList(as:List[Pair[Int,Int]]):Vector[Int] = {
          // (exponent, coefficient)
    println("AssocList: " + as)

    val maxC:Int = as.maxBy(_._1)._1 + 1 // the plus 1 is because the zero counts as a coefficient
    val poly:Array[Int] = new Array[Int](maxC) // the new keyword prevents a single element Array
    println("intArray: " + poly.deep)
    as.foreach( (tpl) => tpl match { case (exp, c) => require(exp >= 0); println("got: " + tpl);poly(exp) = c} )
    val c=poly.toVector.reverse
    println("Vector: " + c)
    c
  }


  def apply (x: Int) = cs.foldRight(0) ((a, c) => a * x + c)

  def + (p: Polynom) = p
  def * (p: Polynom) = p
  def ° (p: Polynom) = p

  override def toString = cs.toString //(cs.zipWithIndex.foldLeft ( ("", 0) ) ( (strExp, c) => (c + "x^" + strExp._2 + " " + strExp._1, strExp._2 + 1))) ._1

}


object Polynom {
  def apply (cFirst: Int, csRest: Int *) = {
    val cs = cFirst :: csRest.toList
    val withExponents = cs.reverse.zipWithIndex.map(_.swap)
    new Polynom(withExponents)
  }
  def apply (cFirst: Pair[Int, Int], cs: Pair[Int, Int]* ) = new Polynom (cFirst :: cs.toList)

}
