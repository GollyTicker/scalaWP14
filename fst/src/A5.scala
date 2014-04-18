/**
 * Created by Swaneet on 18.04.2014.
 */

object Run {
  def run() = {
    println(Action.make("<-", "MC") over MC.inital)  // should be 1 1 ~~~ 2 2
  }
}

object MC {
  lazy val inital = State.make(0,0,MS,CS)

  val MS = 3
  val CS= 3
  val NPPL = CS + MS

  def isGameOver(st:State) = st match {
    case State(ml,cl,mr,cr) => ml < cl || mr < cr
  }

  def isGameFinished(st:State) = st match {
    case State(`MS`,`CS`,0,0) => true   // *almost PROLOG style matching here*
    case _ => false
  }

  def __action_over(act:Action)(st:State):Option[State] = {
    None
  }

}

// missionary left, cannibal left, massionary right, cannibal right
case class State private (val ml:Int, val cl:Int, val mr:Int, val cr:Int) {
  override def toString = ml + " " + cl + " ~~~ " + mr + " " + cr
}
object State {
  def make(ml:Int, cl:Int, mr:Int, cr:Int) = {
    val ls = List(ml,cl,mr,cr)
    assert(ls.sum == MC.NPPL)
    assert(ls.forall( _ >= 0))
    State(ml,cl,mr,cr)
  }
}

// from right to left, passengers
case class Action private (val rtoleft:Boolean, val psg:Boarding) {
  override def toString = if (rtoleft)
                              "<-" + psg + "<-"
                              else "->" + psg + "->"

  def over(st:State):Option[State] = MC.__action_over(this)(st)
}

object Action {
  def make(dirStr:String, psgStr:String) = {
    val rtoleft = if (dirStr=="<-") true
                  else if (dirStr=="->") false
                  else throw new RuntimeException
    val psg = Boarding(psgStr)
    Action(rtoleft, psg)
  }
}

class Boarding private (val str:String) {
  override def toString = str
}

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


