/**
 * Created by Swaneet on 16.05.2014.
 */
object A9 {

  def run() = {
    a1()
  }

  def a1() = {
    val x = new Car(1997,"Ford","E350","ac, abs, moon",3000.00)

    println(x)
    println(csv(x))

  }

}

trait CSV[T] {
  def csv(value :T):String
}

object csv {  // shortcut
  def apply[A:CSV](x: A):String = x.csv(x)
}

class Car(
          val year: Int,
          val make: String,
          val model: String,
          val description: String,
          val price: Double
         )
  extends CSV[Car]
{
  override def toString():String = List("" + year, make, model, description, "" + price).mkString(",")
  def csv(x: Car): String = x.toString
}