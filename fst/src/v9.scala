/**
 * Created by Swaneet on 13.05.2014.
 */
object v9 {

  // Gennerischer Typ
  abstract class Buffer[T] {
    val element:T
  }


  // ein Buffer mit Elementen mit einem noch unbekannten Typ T
  abstract class Buffer2 {
    type T
    val element:T
  }
  // Buffer2 ist mächtiger als Buffer

  abstract class OnlyShapes {
    type S <: Shape
    val s:S
  }

  class Shape
  class Square extends Shape
  class Circle extends Shape

  val s2 = new OnlyShapes {
    val s: S = new Square
    type S = Square
  }

  // weiter konkretisieren
  // immernoch abstrakt, weil "s" nicht definiert ist.
  abstract class WorkOnSquares extends OnlyShapes {
    type S = Shape
  }

  val s3 = new WorkOnSquares { val s: S = new S }

  // ordered Elements
  abstract class OrderedElements {
    type T <: Ordered[T]
  }

  // Typ Projektionen
  // auf die inneren Typen zugreifen

  trait Decimal {
    type F = Float
    type D = Double
  }

  //val f:Decimal#F = 1
  //val d:Decimal#D = 1


  // Path dependent classes.
  // class path1.Point != class path2.Point
  // Funzt aber, wenn man # statt . verwendet: ... val p1 = Path#Point(0,1) ...


  // Immutable Typen: (nur lesen) +
  // Mutalbe Typen: (nur schreiben) -

  // z.B. Tuple[+T1,+T2]
  // z.B. Function[-T1, +R]

  // Ordering implicits
}
