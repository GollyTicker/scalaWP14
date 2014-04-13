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

    // test works, disabled due to 4sec calculation time.
    // test(shortestRoute(List(1,2,3,4)), (133, List(2,4,1,3)) )
  }

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

