/**
 * Created by Swaneet on 01.04.14.
 */
object vorls3 {

  def sumOfProducts(arr1:Array[Double], arr2:Array[Double]) =
     arr1
       .zip(arr2)
       .map((t) => t._1 * t._2)
       .sum

  def product(arr1:Array[Double], arr2:Array[Double]) =
    arr1
      .zip(arr2)
      .map((t) => t._1 * t._2)

  def productFor(arr1:Array[Double], arr2:Array[Double]) = {
    import scala.math.min
    for (i <- 0 until min(arr1.size, arr2.size))
      yield arr1(i) * arr2(i)
  }
}


object util {
  //def zipWith[A,B](xs:List[A], ys:List[A], f:((A) => B))
}


object MyApp {
  def main(args:Array[String]){
    println(args.deep.toString)
  }
}



// Companion Object
// wie man statische methoden die in eineer Klasse sind,
// in singletons rausnehmen kann.
// Factories erstellen


/*class HasComp private() { // constructor made private
  print("constructor")
  override def toString():String = "HasComp"
}

object HasComp {
  def create = new HasComp

*/



class User(val name:String)

trait Mail {
  val provider = "t-online"
  val user:String
  def address = user + "@" + provider // ein def wird immer wieder evaluiert
                                      // ein val wird genau einmmal evaluiert und dann memoisiert
}

trait Bank {
  def account: Long
}

class Customer(name:String, val user:String, val account:Long) extends User(name) with Mail with Bank {
  override def toString:String = name + "," + user + "," + account
}

object Cust {
  def apply():Customer = {
    val c = new Customer("Müller", "Ute.Müller", 123456789)
    c
  }
}
// abc(1,2,3) => abc.apply(1,2,3)
// Funktion und Lambda


