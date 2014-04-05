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
    val myPoly = Polynom(4,14,(-2),15)

    println("myPoly:" + myPoly)
    println("myPoly(3):" + myPoly(3))
  }


}

class Polynom private (csAssoc: List[Pair[Int,Int]] ) {

  val cs = fromAssocList(csAssoc)
  val len = cs.length

  def fromAssocList(as:List[Pair[Int,Int]]):Vector[Int] = {
          // (coefficient, exponent)
    println("AssocList: " + as)

    val maxC:Int = as.maxBy(_._2)._2 + 1 // the plus 1 is because the zero counts as a coefficient
    val poly:Array[Int] = new Array[Int](maxC) // the new keyword prevents a single element Array
    println("intArray: " + poly.deep)
    as.foreach( (tpl) => tpl match { case (c, exp) => require(exp >= 0); poly(exp) = c} )
    val c=poly.toVector
    println("Vector: " + c)
    c
  }


  def apply (x: Int) = cs.foldRight(0) ((c, accu) => accu * x + c)

  def + (p: Polynom) = p
  def * (p: Polynom) = p
  def ° (p: Polynom) = p

  override def toString():String = {
    def step(str:String, tpl:Pair[Int,Int]):String =  tpl._1.toString() + "x^" + tpl._2.toString() + " " + str
    cs.zipWithIndex
      .foldLeft("")((a, b) => step(a, b))
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
