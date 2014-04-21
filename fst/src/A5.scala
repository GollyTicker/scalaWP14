/**
 * Created by Swaneet on 18.04.2014.
 */

object Run {
  def run() = {

    val solution:List[Action] = List("<-","->","<-","->","<-","->","<-","->","<-","->","<-")
                          .zip(List("MC", "M","CC", "C","MM","MC","MM", "C","CC", "C","CC"))
                          .map( tpl => Action(tpl._1, tpl._2) )

    println("Beginning: " + MC.startGame(List()))
    println("Sample Move: " + MC.play("<-", "MC")(MC.beginning))
    println("Ending: " + MC.startGame(solution))
  }
}

object MC {
  lazy val initial = State.make(0,0,MS,CS, false)
  lazy val beginning = initial lift

  val MS = 3
  val CS= 3
  val NPPL = CS + MS

  type GameProgress = Option[(State,String)]
    // None => invalid Turn
    // Some(st. "W") => won
    // Some(st, "C") => to be continued
    // Some(st, "L") => game over (lost)

  def debug(str:String):Unit = println(str)

  def isGameOver(st:State) = st match {
    case State(ml,cl,mr,cr, isLeft) => 0 < ml && ml < cl || 0 < mr && mr < cr
  }

  def isGameFinished(st:State) = st match {
    case State(`MS`,`CS`,0,0,_) => true   // *almost PROLOG style matching here*
    case _ => false
  }

  // thows away the progress, because one may not do any action now,
  // unless the game is still to be continued ("C")
  // if the game is to be continued, then the action is executed
  def play_(act:Action)(prev:GameProgress):GameProgress = {
    prev match {
      case None => {debug("play_1: cannot process because invalid Progress"); None}
      case Some((_, "W")) => {debug("play_2: Game is already won."); None}
      case Some((_, "L")) => {debug("play_3: Game is already over."); None}
      case Some((st, "C")) => st takes act  // here the action is executed
      case Some((_, _)) => throw new RuntimeException
    }
  }
  // play is the user friendly variant. play_ is the actual implementation
  def play(dirs:String, psgs:String) = play_(Action(dirs,psgs)) _

  def startGame = playFrom(beginning) _

  // playes the actiosn from a given Progress.
  def playFrom(game:GameProgress)(ls:List[Action]):GameProgress = ls.foldLeft(game)( (yet,act) => play_(act)(yet) )

  // the actual implementation of game logic.
  def _action_over_state(act:Action)(st:State):GameProgress = {
    val lch:Int => Int = x => x + (if (act.rtoleft) (1) else (-1))
    val rch:Int => Int = x => x + (if (act.rtoleft) (-1) else (1))

    if (act.rtoleft == !st.isLeft) ( () ) // all ok!
              else return {debug("_action_over_state: boat wrong"); None}  // bad. wrong direction

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
      case State(ml, cl, mr, cr, isLeft) => {
        val newML = mlch(ml)
        val newCL = clch(cl)
        val newMR = mrch(mr)
        val newCR = crch(cr)
        val newIsLeft = !isLeft
        State.safeMake(newML, newCL, newMR, newCR, newIsLeft)
      }
    }
    handleIfEndOfGame(newOptionState)
  }

  def handleIfEndOfGame(newOptSt:Option[State]):GameProgress =
    for {
        newSt <- newOptSt
        result = () match { // using match expression because "if else if else" looks ugly.
          case _ if MC.isGameOver(newSt) => (newSt, "L")
          case _ if MC.isGameFinished(newSt) => (newSt, "W")
          case _ => (newSt, "C")
        }
    } yield result

  // for a given game progress, "solve" returns a list of actions for which this game
  // can be finished properly. If this isnt possible, then it returns None.
  def solve(yet:GameProgress):Option[List[Action]] = None
}
// missionary left, cannibal left, massionary right, cannibal right
case class State private (val ml:Int, val cl:Int, val mr:Int, val cr:Int, val isLeft:Boolean) {
  override def toString = {
    val riverAndBoat = if (isLeft) "<>~~~  " else "  ~~~<>"
    ml + " " + cl + "  " + riverAndBoat + " " + mr + " " + cr
  }
  def takes(act:Action):MC.GameProgress = MC._action_over_state(act)(this)
  lazy val lift:MC.GameProgress = MC.handleIfEndOfGame(Some(this))
}

object State {
  // not using apply here, because I want a return type of Option[State] (see safeMake) insteadt of State.
  def make(ml:Int, cl:Int, mr:Int, cr:Int, isLeft:Boolean) = {
    safeMake(ml,cl,mr,cr, isLeft).get
  }

  def safeMake(ml:Int, cl:Int, mr:Int, cr:Int, isLeft:Boolean):Option[State] = {
    val ls = List(ml,cl,mr,cr)
    if (ls.sum != MC.NPPL) return {MC.debug("safeMake1"); None}
    if (ls.exists( _ < 0)) return {MC.debug("safeMake2");None}
    Some(State(ml,cl,mr,cr,isLeft))
  }
}

// from right to left, passengers
class Action private (val rtoleft:Boolean, val psg:Boarding) {
  override def toString = if (rtoleft)
                              "<-" + psg + "<-"
                              else "->" + psg + "->"
}

object Action {
  def apply(dirStr:String, psgStr:String) = {
    val rtoleft = if (dirStr=="<-") true
                  else if (dirStr=="->") false
                  else throw new RuntimeException
    val psg = Boarding(psgStr)
    new Action(rtoleft, psg)
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


