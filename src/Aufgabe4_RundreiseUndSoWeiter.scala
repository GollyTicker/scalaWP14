import scala.annotation.tailrec
import scala.compat.Platform

/**
 * Created by Swaneet on 11.04.2014.
 */
object A {

  def run() {
    var failed = false
    // helper function to display test results
    def test(a: Any, b: Any) = {
      val tpl = if (a == b) {
        (" == ", "")
      }
      else {
        failed = true
        (" /= ", "\nExpected: " + a + "\nResult:" + b)
      }
      println(a.toString() + tpl._1.toString() + b.toString() + tpl._2.toString())
    }

    // shortestRoute test
    test(A1.shortestRoute(List(1, 2, 3, 4)), (133, List(2, 4, 1, 3)))

    // Run Length encoding tests
    test(A2.RLE("".toList), List())
    test(A2.RLE(List()), List())
    test(A2.RLE("DEEEEffaSSSW".toList), List(('D', 1), ('E', 4), ('f', 2), ('a', 1), ('S', 3), ('W', 1)))
    test(A2.RLE(List(1, 1, 1, 1, 0, 0, 0, 1, 0)), List((1, 4), (0, 3), (1, 1), (0, 1)))


    // Mixin traits: A4
    val pers1 = new A4.Person1("Schmidt")
    val pers2 = new A4.Person2("Schmidt")
    val pers3 = new A4.Person("Müller") with A4.Student with A4.Lecturer with A4.Worker {
      override def work = super[Student].work + " and " + super[Lecturer].work // use super[TraitName] to refer to a specific trait.
    }
    test(pers1 + ": " + pers1.work, "Person(Schmidt): I am lecturing")
    test(pers2 + ": " + pers2.work, "Person(Schmidt): I am studying")
    test(pers3 + ", " + pers3.act, "Person(Müller), I am studying and I am lecturing")

    println("Testcases " + (if (failed) "failed." else "succeeded."))
  }
}

object A1 {
  def distance(tpl: (Int, Int)): Int = {
    val c1 = tpl._1
    val c2 = tpl._2

    val diff = if (c1 > c2) c1 - c2 else c2 - c1
    100 / diff
  }

  def shortestRoute(ls: List[Int]): (Int, List[Int]) = {
    require(!ls.isEmpty)
    def calcDist(ps: List[Int]): (Int, List[Int]) = {
      // calculate the distance between consecutive elements and sum the distances
      // val dist = ps.zip(ps.tail).map(distance _).sum // Swaneet
      val dist = ps.sliding(2).map(ls => distance( (ls(0), ls(1))) ).sum  // Esser: Sliding.
                        // sliding(n) erzeugt ein Fenster aller n-consecutive Elements und gibt eine Liste dessen zurück.
      (dist, ps)
    }
    ls
      .permutations // all Tours
      .map(calcDist) // assign a cost to each tour
      .minBy(_._1) // take the minimal cost. (miyBy already take the first if there are multiple minima)
  }
}

object A2 {

  // in the Run Length Encoding methods the second element in the tuple
  // represents the number of consecutive occurences of that singleton.

  def RLE[A](xs: List[A]): List[(A, Int)] = group(xs).map(x => (x.head, x.size))

  // how come this isnt in scala.List.immutable?
  // groups equal elements together: "AAffeDD" => ["AA", "ff", "e", "DD"]
  def group[A](xs: List[A]): List[List[A]] = xs match {
    case Nil => List()
    case _ => {
      val fst = xs.head
      val tpl = xs.span(_ == fst) // xs.span(f) == (xs takeWhile f, xs dropWhile f)
      tpl._1 :: group(tpl._2)
    }
  }

}

object A3 {
  def run = {
    println( "Outerthread: " +
      async {
          println("Innerthread: I am very sleepy")
          // Thread.sleep(2000)
          println("Innerthread: " + (Polynom(4, 3, 2, 1) * Polynom(1, 2, 3, 4))(-1))
        }
      )
    println("Outerthread: " + Polynom(4, 3, 2, 1) * Polynom(1, 2, 3, 4))
    println("Outerthread: stopped")
  }
  // Expected output:
  /*
    I am very sleepy
    Thread[Thread-0,5,main]
    Polynom(4x⁶ + 11x⁵ + 20x⁴ + 30x³ + 20x² + 11x + 4)
    stopped
    -4
  */
  def async[A](f: => A) = {
    //val hello = new Thread(new Runnable{ def run() = body})
    //hello.start()
    //hello
  }

}

object A4 {

  case class Person(name: String)

  trait Student {
    def work = "I am studying"
  }

  trait Lecturer {
    def work = "I am lecturing"
  }

  trait Worker {
    def work: String

    def act = work // ???
  }

  class Person1(name: String) extends Person(name) with Student with Lecturer {
    override def work = super.work // super steht hier für (Person + Student + Lecturer)   (?)
  }

  class Person2(name: String) extends Person(name) with Lecturer with Student {
    override def work = super.work
  }

}




