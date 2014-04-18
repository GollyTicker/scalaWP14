/**
 * Created by Swaneet on 18.04.2014.
 */

object Run {
  def run() = {
    println(MC.initial takes Action.make("<-", "MC"))  // should be 1 1 ~~~ 2 2
  }
}

object MC {
  lazy val initial = State.make(0,0,MS,CS)

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

  def chain(act:Action)(prev:GameProgress):GameProgress = {
    prev match {
      case None => None
      case Some((_, "W")) => None
      case Some((_, "L")) => None
      case Some((st, "C")) => st takes act
      case Some((_, _)) => throw  new RuntimeException
    }
  }

  type GameProgress = Option[(State,String)]
        // None => invalid Turn
        // Some(st. "W") => won
        // Some(st, "C") => to be continued
        // Some(st, "L") => game over (lost)

  def _action_over_state(act:Action)(st:State):GameProgress = {
    val lch:Int => Int = x => x + (if (act.rtoleft) (1) else (-1))
    val rch:Int => Int = x => x + (if (act.rtoleft) (-1) else (1))

    // the ch stands for "change". mlch means the change in numbers of missionaries on the left side
    var mlch:Int => Int = x => x
    var clch:Int => Int = x => x
    var mrch:Int => Int = x => x
    var crch:Int => Int = x => x

    // assign the functions according to the boat
    act.psg.str match {
      case "M" => {mlch = lch; mrch = rch}
      case "C" => {clch = lch; crch = rch}
      case "MC" => {mlch = lch; mrch = rch;  clch = lch; crch = rch}
      case "MM" => {mlch = lch compose lch; mrch = rch compose rch}
      case "CC" => {clch = lch compose lch; crch = rch compose rch}
    }

    val newOptionState:Option[State] = st match {
      case State(ml, cl, mr, cr) => {
        val newML = mlch(ml)
        val newCL = clch(cl)
        val newMR = mrch(mr)
        val newCR = crch(cr)
        State.safeMake(newML, newCL, newMR, newCR)
      }
    }
    checkEnding(newOptionState)
  }

  def checkEnding(newOptSt:Option[State]):GameProgress =
    for {
        newSt <- newOptSt
        result = () match { // because if else if else look so ugly here
          case _ if MC.isGameOver(newSt) => (newSt, "L")
          case _ if MC.isGameFinished(newSt) => (newSt, "W")
          case _ => (newSt, "C")
        }
    } yield result

}

// missionary left, cannibal left, massionary right, cannibal right
case class State private (val ml:Int, val cl:Int, val mr:Int, val cr:Int) {
  override def toString = ml + " " + cl + " ~~~ " + mr + " " + cr
  def takes(act:Action):MC.GameProgress = MC._action_over_state(act)(this)
  lazy val lift:MC.GameProgress = MC.checkEnding(Some(this))
}

object State {

  def make(ml:Int, cl:Int, mr:Int, cr:Int) = {
    safeMake(ml,cl,mr,cr).get
  }

  def safeMake(ml:Int, cl:Int, mr:Int, cr:Int):Option[State] = {
    val ls = List(ml,cl,mr,cr)
    if (ls.sum != MC.NPPL) return None
    if (ls.exists( _ < 0)) return None
    Some(State(ml,cl,mr,cr))
  }
}

// from right to left, passengers
case class Action private (val rtoleft:Boolean, val psg:Boarding) {
  override def toString = if (rtoleft)
                              "<-" + psg + "<-"
                              else "->" + psg + "->"
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


