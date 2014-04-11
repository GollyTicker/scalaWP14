import scala.compat.Platform

/**
 * Created by Swaneet on 11.04.2014.
 */
object A {

  def distance(c1: Int, c2: Int):Double = {
    if (c1 == c2) return 0
    val diff = if (c1>c2) c1 - c2 else c2 - c1
    100 / diff
  }

  def shortestRoute(ls:List[Int]):(Double, List[Int]) = {

    val init = (Double.PositiveInfinity, Nil)

    def step (tpl:(Double, List[Int])) (x:Int):(Double, List[Int]) = tpl match {
      case (currMin, currMinList) => (currMin, currMinList)
    }

    ls.permutations.foldLeft(init)( step(_)(_) )
  }

}

