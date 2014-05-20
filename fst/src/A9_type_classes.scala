/**
 * Created by Swaneet on 16.05.2014.
 */
object A9 {

  def run() = {
    a1()

    // a2()
  }

  def a1() = {
    import CarCSV._
    val x = new Car(1997, "Ford", "E350", "ac, abs, moon", 3000.00)
    println(x)
    println(csv(x))
  }
}

trait CSV[T] {
  def toCSV(value: T): String
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

object CarCSV {
  // hier der typeclass instance implementation (außerhalb der KLassendefinition)
  implicit val carCSV = new CSV[Car] {
    def toCSV(c: Car) = c.toString()
  }

  // useCsv ist die Methode die vom Andender verwendet wird. Sie besagt, dass ich den Kontext von CSV meine.
  def csv[T: CSV](c: T) = implicitly[CSV[T]].toCSV(c)

}