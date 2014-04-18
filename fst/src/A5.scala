/**
 * Created by Swaneet on 18.04.2014.
 */
object MC {
  lazy val inital = State(0,0,3,3)

  def isGameOver(st:State) = st match {
    case State(ml,cl,mr,cr) => ml < cl || mr < cr
  }

  def __action_over(act:Action)(st:State) = throw new RuntimeException

}

// missionary left, cannibal left, massionary right, cannibal right
case class State(val ml:Int, val cl:Int, val mr:Int, val cr:Int) {
  override def toString = ml + " " cl + " ~~~ " + mr + " " + cr
}
// from right to left, passengers
case class Action(val rtoleft:Boolean, val psg:Boarding) {
  override def toString = if (rtoleft)
                              "<-" + psg + "<-"
                              else "->" + psg + "->"
  def over(st:State) = MC.__action_over(this)(st)
}

class Boarding private (val str:String)

object Boarding {
  def apply(psg:String) = new Boarding (psg match {
    case "M" => psg
    case "C" => psg
    case "MC" => psg
    case "MM" => psg
    case "CC" => psg
    case _ => throw new RuntimeException
  })
}


