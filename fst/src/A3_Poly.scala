/**
 * Created by Swaneet on 05.04.2014.
 */
object A3_Poly extends App {


  def test(a: AnyRef, b: AnyRef) = {
    val tpl = if (a == b) (" ==","") else ("/=", "\nExpected: " + a + "\nResult:" + b
    "a " + tpl._1 + " b" + tpl._2
  }

class Polynom private (val cs: List[Int] ) {
  def apply (x: Int) = cs.foldLeft (0) ((a, c) => a * x + c)

  def + (p: Polynom) = p
  def * (p: Polynom) = p
  def ° (p: Polynom) = p

}


object Polynom {
  def apply (cFirst: Int, cs: Int *) = new Polynom (cFirst :: cs.toList)
  def apply (cFirst: Pair[Int, Int], cs: Pair[Int, Int] ) = new Polynom (0 :: Nil) //TODO: allow tuplesd input

}
