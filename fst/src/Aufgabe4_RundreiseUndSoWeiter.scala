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
    test(A1.shortestRoute(List(1,2,3,4)), (133, List(2,4,1,3)) )

    // Run Length encoding tests
    test(A2.RLE("".toList), List())
    test(A2.RLE(List()), List())
    test(A2.RLE("DEEEEffaSSSW".toList), List( ('D',1), ('E',4), ('f',2), ('a',1), ('S',3), ('W',1) ))
    test(A2.RLE(List(1,1,1,1,0,0,0,1,0)), List( (1,4), (0,3), (1,1), (0,1) ))

    println( "Testcases " + (if (failed) "failed." else "succeeded.") )
  }


}

object A2 {

  // in the Run Length Encoding methods the second element in the tuple
  // represents the number of consecutive occurences of that singleton.

  def RLE[A](xs:List[A]):List[(A, Int)] = group(xs).map( x => (x.head, x.size))

  // how come this isnt in scala List immutable?
  // groups equal elements together: "AAffeDD" => ["AA", "ff", "e", "DD"]
  def group[A](xs:List[A]):List[List[A]]= xs match {
    case Nil => List()
    case _ => {
      val fst = xs.head
      val tpl = xs.span(_ == fst)
      tpl._1 :: group(tpl._2)
    }
  }

}

object A1 {
  def distance(tpl:(Int,Int)):Int = {
    val c1 = tpl._1
    val c2 = tpl._2

    val diff = if (c1>c2) c1 - c2 else c2 - c1
    100 / diff
  }

  def shortestRoute(ls:List[Int]):(Int, List[Int]) = {
    require( ! ls.isEmpty )
    def calcDist(ps:List[Int]):(Int, List[Int]) = {
      val dist = ps.zip(ps.tail).map( distance _ ).sum
      (dist, ps)
    }
    ls
      .permutations     // all Tours
      .map( calcDist )  // assign a cost to each tour
      .minBy( _._1 )    // take the first with minimal cost.
  }
}

