/**
 * Created by Swaneet on 08.04.2014.
 */
object vorls4 {

  import scala.math._
  def sqr = (x:Double) => sqrt(x)

  def neg = (x:Double) => -x

  // Variable Args

  def map[A](f:String => A, strs:String*) =
                              for {
                                    s <- strs
                              } yield (f(s))

  def app[A,B](f: A => B, x:A):B = f(x)

  def f = (g:Int => Int, h:Int => Double) => h compose g

  def printFunction( f:Double => Double, xs:Double*):Unit = { xs.map( ( (x) => println("f("+ x +") = " + f(x))) ); ()}

  // curryting
  def bezier(d:Double)(c:Double) = d + c

  def mult(x:Int)(y:Int) = x*y
  // mult: (x: Int)(y: Int)Int

  val div = (x:Int) => (y:Int) => x / y
  // div: Int => (Int => Int) = <function1>

  val mult1 = mult(10)_
  // mult1: Int => Int = <function1>

  val multiply = (x:Int,y:Int) => mult(x)(y)
  // multiply: (Int, Int) => Int = <function2>

  val multiply2 = (x:Int,y:Int) => { import scala.Function.uncurried; uncurried(mult _)}
  // multiply: (Int, Int) => Int = <function2>

  /*
  * scala> multiply(4,5)
  * res9: Int = 20
  *
  * scala> multiply.curried
  * res10: Int => (Int => Int) = <function1>
  *
  * scala> multiply.curried (4) (5)
  * res11: Int = 20
  * */
  def poly(coeff:Double*)(x:Double) = coeff.foldLeft(0.0)(_ * x + _)
  // poly: (coeff: Double*)(x: Double)Double

  // poly(2,-1,1)(10)
  // res10: Double = 191.0 // ist korrekt
 }
