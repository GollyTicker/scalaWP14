/**
 * Created by Swaneet on 05.04.2014.
 */
object A3_Poly extends App {


  def test(a: AnyRef, b: AnyRef) = {
    val tpl = if (a == b)
      (" == ", "")
    else (" /= ", "\nExpected: " + a + "\nResult:" + b)
    "a" + tpl._1 + "b" + tpl._2
  }
}

class Polynom private (csAssoc: List[Pair[Int,Int]] ) {

  val cs = fromAssocList(csAssoc)

  def fromAssocList(as:List[Pair[Int,Int]]):List[Int] = {
    Nil
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
