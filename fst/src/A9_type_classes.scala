/**
 * Created by Swaneet on 16.05.2014.
 */
object A9 {

  def run() = {
    a1()

    A9_A3.a3()
  }

  def a1() = {
    import CarCSV._
    val x = new Car(1997, "Ford", "E350", "ac, abs, moon", 3000.00)
    println(x)
    println(csv(x))

  }

  def a2() = {
    import MatrixImplicits._
    val mat = Matrix(1, 2, 3, 4, 5, 6, 7, 8, 9)
    // println(mat * 5 == 5 * mat) // ???
  }


}

trait CSV[T] {
  def csv(value: T): String
}

trait Scale[T] {
  def *(s: Double, x: T): T
}

object MatrixImplicits {

  implicit object MatrixScale extends Scale[Matrix] {
    def *(s: Double, x: Matrix): Matrix = x * s
  }

  implicit object MatrixNumeric extends math.Numeric[Matrix] {

    def plus(x: Matrix, y: Matrix): Matrix = x + y

    def toDouble(x: Matrix): Double = x(0, 0)

    def toFloat(x: Matrix): Float = toDouble(x).toFloat

    def toLong(x: Matrix): Long = toDouble(x).toLong

    def toInt(x: Matrix): Int = toDouble(x).toInt

    def fromInt(x: Int): Matrix = Matrix(1, x)

    def negate(x: Matrix): Matrix = -x

    def times(x: Matrix, y: Matrix): Matrix = x * y

    def minus(x: Matrix, y: Matrix): Matrix = x - y

    def compare(x: Matrix, y: Matrix): Int = ???
  }

}

class Car(
           val year: Int,
           val make: String,
           val model: String,
           val description: String,
           val price: Double
           ) {
  override def toString(): String = List("" + year, make, model, description, "" + price).mkString(",")
}

object CarCSV extends CSV[Car] {
  // hier der typeclass instance (außerhalb der KLassendefinition)
  def csv(c: Car): String = c.toString()
}

trait BaseUnit

trait Meter extends BaseUnit

trait Kelvin extends BaseUnit

trait Second extends BaseUnit

trait Quantity {
  type unit <: BaseUnit
  def value: Double
}

trait QMonoid[Q <: Quantity] {
  def plus(x: Q, y: Q): Q
}

case class Length(val d:Double)
case class Temperature(val d:Double)
case class Time(val d:Double)


object unitImplicits {
  implicit class Length2(val d:Double) extends Quantity {
    type unit = Meter
    val value = d
  }
  implicit class Temperature2(val d:Double) extends Quantity {
    type unit = Kelvin
    val value = d
  }
  implicit class Time2(val d:Double) extends Quantity {
    type unit = Second
    val value = d
  }

}

def add[Q <: Quantity : QMonoid](x: Q, y: Q): Q = implicitly[QMonoid[Q]].plus(x, y)

object A9_A3 {
  def a3() = {
    import unitImplicits._
    println(add(Length(1), Length(2.7)))
    println(add(Temperature(273.15), Temperature(30.0)))
    println(add(Time(100), Time(50)))
    println(Length(1) + Length(2.7))
    println(Temperature(273.15) + Temperature(30.0))
    println(Time(100) + Time(50))
  }
}