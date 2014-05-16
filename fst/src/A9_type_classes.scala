/**
 * Created by Swaneet on 16.05.2014.
 */
object A9 {

  def run() = {
    a1()
  }

  def a1() = {
    import CarCSV._
    val x = new Car(1997, "Ford", "E350", "ac, abs, moon", 3000.00)
    println(x)
    println(csv(x))

  }
}

object CSV {

  trait CSV[T] {
    def csv(value: T): String
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

object CarCSV extends CSV.CSV[Car] {
  // hier der typeclass instance (außerhalb der KLassendefinition)
  def csv(c: Car): String = c.toString()
}