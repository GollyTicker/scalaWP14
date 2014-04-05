/**
 * Created by Swaneet on 05.04.2014.
 */
object A3_Poly{


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

  def fromAssocList(as:List[Pair[Int,Int]]):Vector[Int] = {
    println("AssocList: " + as)

    val maxC:Int = as.maxBy(_._1)._1
    val poly:Array[Int] = new Array(maxC) // the new keyword prevents a single element Array
    as.foreach( (tpl) => tpl match { case (exp, c) => require(exp >= 0); poly(exp) = c} )
    val c=poly.toVector
    println("Vector: " + c)
    c
  }

  def apply (x: Int) = cs.foldLeft (0) ((a, c) => a * x + c)

  def + (p: Polynom) = p
  def * (p: Polynom) = p
  def ° (p: Polynom) = p

}


object Polynom {
  def apply (cFirst: Int, csRest: Int *) = {
    val cs = cFirst :: csRest.toList
    new Polynom(cs.reverse.zipWithIndex)
  }
  def apply (cFirst: Pair[Int, Int], cs: Pair[Int, Int]* ) = new Polynom (cFirst :: cs.toList)

}
