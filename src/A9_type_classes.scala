import com.sun.javafx.tk.quantum.QuantumClipboard

/**
 * Created by Swaneet on 16.05.2014.
 */
object A9 {

  def run() = {
    a1()

    baseUnitAufgabe.a3()
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
  // hier der typeclass instance implementation (außerhalb der Klassendefinition)
  implicit val carCSV = new CSV[Car] {
    def toCSV(c: Car) = c.toString()
  }

  // useCsv ist die Methode die vom Andender verwendet wird. Sie besagt, dass ich den Kontext von CSV meine.
  def csv[T: CSV](c: T) = implicitly[CSV[T]].toCSV(c)
}


// Aufgabe 3

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

// ================== NEU HINZUGEFÜGT ==========================
object Implicits {

  // die Methode die der User aufruft
  def add[Q <: Quantity : QMonoid](x: Q, y: Q): Q = implicitly[QMonoid[Q]].plus(x, y)

  // allgemeine Implementation fur Quantities. Ein Konstruktor muss mit übergeben werden.
  def QMonoidOver[Q <: Quantity](create:Double => Q): QMonoid[Q] = new QMonoid[Q] {
    def plus(x: Q, y: Q): Q = create(x.value + y.value)
  }
  // erlaubt plus(a,b) auch als a + b zu schreiben
  implicit class plusImplicits[Q<:Quantity:QMonoid](x:Q) {
    def +(y:Q) = implicitly[QMonoid[Q]].plus(x,y)
  }

  // jetzt kann man den Quantities implementationen hinzufügen
  implicit def LengthMonoid:QMonoid[Length] = QMonoidOver(Length)

  implicit def TemperatureMonoid:QMonoid[Temperature] = QMonoidOver(Temperature)

  implicit def TimeMonoid:QMonoid[Time] = QMonoidOver(Time)

  /*
  // manuelle TypeClass implementation
  implicit def LengthMonoid: QMonoid[Length] = new QMonoid[Length] {
    def plus(x: Length, y: Length): Length = Length(x.value + y.value)
  }
  */
}

// Die Quantity Klassen
case class Length(x: Double) extends AnyRef with Quantity { // wie ist die Syntax, wenn man nur ein trait implementieren will?
  type unit = Meter
  def value: Double = x
}

case class Temperature(x: Double) extends AnyRef with Quantity { type unit = Kelvin; def value: Double = x }

case class Time(x: Double) extends AnyRef with Quantity { type unit = Second; def value: Double = x }

// ============ ENDE - NEU HINZUGEFÜGT - ENDE ===================

object baseUnitAufgabe {
  def a3() {
    import Implicits._

    println(add(Length(1), Length(2.7)))
    println(add(Temperature(273.15), Temperature(30.0)))
    println(add(Time(100), Time(50)))

    println((Length(1)+Length(2.7)))
    println(Temperature(273.15)+Temperature(30.0))
    println(Time(100)+Time(50))

    // add(Length(1),Time(2))  // <- compiliert nicht
    // Length(1) + Time(2)  // <- compiliert nicht

    // this is a "shapeless non-compilaton" example !!
    // https://github.com/milessabin/shapeless/wiki/Feature-overview:-shapeless-2.0.0#testing-for-non-compilation
  }
}



object Reverse {
  // alles Reversable A hat
  trait Reverse[A] {
    type B  // einen Reverse Typ (muss nicht gleich A sein, da z.B. Pair[A,B] => Pair[B,A])
    def reverse(a:A):B  // eine reverse Methode
  }

  // Zwei Arten der Definition:

  // 1. Haskell like Polymorhpic Constraint
  // def reverse[A:Reverse](x:A) = implicitly[Reverse[A]].reverse(x)
  // Aber in der Instance von Reverse[Int]: def reverse(x:Int):B = Integer.parseInt(Reverse.reverse(x.toString))
  // geht der return type String verloren.... muss dann manuell zu String gecastet werden.....

  // daher lieber

  // 2. With Implicit Argument
  def reverse[A](x:A)(implicit revA:Reverse[A]) = revA.reverse(x)


  // Instances:
  implicit val revUnit = new Reverse[Unit] {
    type B = Unit
    def reverse(x:Unit):B = x
  }

  implicit val revString = new Reverse[String] {
    type B = String
    def reverse(x:String):B = x.reverse // ist bereits in String implementiert
  }

  implicit val revInt = new Reverse[Int] {
    type B = Int
    def reverse(x:Int):B = Integer.parseInt(Reverse.reverse(x.toString))
  }

  implicit def revTuple2[T1,T2](implicit revT1:Reverse[T1], revT2:Reverse[T2]) =
    new Reverse[(T1,T2)] {
      type B = (revT2.B, revT1.B) // swap positions
      def reverse(t:(T1,T2)):B = {
        import Reverse.{reverse => innerRev}  // avoiding recursive calling....
        val t1 = innerRev(t._1)
        val t2 = innerRev(t._2)
        (t2,t1) // IDEA meldet fehler obwohl es richtig compiliert
      }
  }

  implicit  def revList[A](implicit revA:Reverse[A]) = new Reverse[List[A]] {
    type B = List[revA.B]
    def reverse(ls:List[A]):B = {
      import Reverse.{reverse => innerRev}
      ls.map( (x:A) => innerRev(x).asInstanceOf[revA.B] ).reverse
    }
  }

  //implicit def rev

  //implicit def revFunctor[F[_], A](implicit revF:Reverse[F], revA:Reverse[A]) = ???
}

object R {
  def main(args:Array[String]) {
    import Reverse._
    val ls = List(
       reverse( () )
      ,reverse( 12431 )
      ,reverse( "HalloWelt!" )
      ,reverse( ("Denkste!", 12345) )
      ,reverse( List( (123,"vfs"), (312, "sfv"), (15243, "awdrg") ) )
    )
    ls foreach println
  }
}


