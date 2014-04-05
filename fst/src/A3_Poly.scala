/**
 * Created by Swaneet on 05.04.2014.
 */
object A3_Poly extends App {

  // val err= Polynom() <- compiliert nicht
  val zero= Polynom(0)
  val one= Polynom(0,0,0,1)
  val p1= Polynom(0,0,0,1,1)
  val p2= Polynom(4,3,2,1)
  val p3= Polynom(3,0,5)
  val p4= Polynom(-1,1,-10)
  val p5= Polynom((50,10))

  /*println(zero +", "+one)
  println(zero ° p2)
  println(p2 ° zero)
  println(p2 ° p3)
  println(p2 * p3)
  println((p2 ° p3)(-1))
  println((p2 * p3)(-1))
  println(p1 * p2)
  println(p3*p4)
  println(p1 +" + "+p2 + " = "+ (p1+p2))
  println((p1+p2)(1))
  println(p1°p2 +"(1)= "+(p1°p2)(1))
  println(p2°p1 +"(1)= "+(p2°p1)(1))
  println((p1*p2)(1))
  println((p2*p1)(1))
  println(p5)

  println((Polynom(4,3,2,1)*Polynom(1,2,3,4))(-1)) == -4
  println(Polynom(4,3,2,1)*Polynom(1,2,3,4)) == Polynom(4x⁶ + 11x⁵ + 20x⁴ + 30x³ + 20x² + 11x + 4)
  */

  /*
  Polynom(0), Polynom(1)
  Polynom(0)
  Polynom(1)
  Polynom(108x⁶ + 567x⁴ + 996x² + 586)
  Polynom(12x⁵ + 9x⁴ + 26x³ + 18x² + 10x + 5)
  2257
  -16
  Polynom(4x⁴ + 7x³ + 5x² + 3x + 1)
  Polynom(-3x⁴ + 3x³ -35x² + 5x -50)
  Polynom(x + 1) + Polynom(4x³ + 3x² + 2x + 1) = Polynom(4x³ + 3x² + 3x + 2)
  12
  Polynom(4x³ + 3x² + 2x + 2)(1)= 11
  Polynom(4x³ + 15x² + 20x + 10)(1)= 49
  20
  20
  Polynom(10x⁵⁰)
  * */
}

class Polynom private(){

  def apply(x:Int) = 0

  def +(p:Polynom) = p
  def *(p:Polynom) = p
  def °(p:Polynom) = p

}


object Polynom {
  def apply(cs:Int*) = new Polynom()

}
