/**
 * Created by Swaneet on 01.04.14.
 */


// komischer IDEA error. Scala funzt hier normal.

trait Shape {
  val basePoint: Point
}


trait ClosedShape extends Shape {
  val area: Double
}

class UseShape[+A <: Shape](sh: A){
  override def toString = {
    val shString:String = sh match {
      case Circle(c, r) if (r >= 5) => "circle mit r => 5"
      case Circle(c, r) if (r < 5) => "circle mit r < 5"
      case closed:ClosedShape => "unbekanntes closed shape mit area = " + closed.area
      case _ => "unbekanntes Shape" // hier könnte auch "case sh:Shape => " hin,
                                    // da die Shape eingrenzung bereits im Tpeparameter festgelegt ist
    }
    "basepoint at: " + sh.basePoint + " -> " + shString
  }
}

case class Point(x: Int, y: Int)

case class Line(fst: Point, snd: Point) extends Shape {
  val basePoint = fst
}

case class Circle(center: Point, r: Double) extends ClosedShape {
  val basePoint:Point = center
  import scala.math.Pi
  val area:Double = r*r*Pi
}

case class Rect(topleft:Point, botright:Point) extends ClosedShape {
  val basePoint = topleft
  import scala.math.abs
  val area:Double = abs(botright.x - topleft.x)*abs(botright.y - topleft.y)
}

object A2_Shape {
  def useAllShapes(u:UseShape[Shape]) = {println(u)}

  def test01 {
    val line = new UseShape(Line(Point(0, 0), Point(1, 1)))
    val circle1 = new UseShape(Circle(Point(1, 2), 10))
    val circle2 = new UseShape(Circle(Point(2, 1), 4))

    val closedShape = new UseShape(Rect(Point(1,1), Point(3,6)))

    useAllShapes(new UseShape(new ClosedShape {val area: Double = 20.0
      val basePoint: Point = Point(2,2)
    }))
    useAllShapes(closedShape)
    useAllShapes(line)
    useAllShapes(circle1)
    useAllShapes(circle2)
  }
}