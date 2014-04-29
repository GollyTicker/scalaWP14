import scala.annotation.tailrec

/**
 * Created by Swaneet on 29.04.2014.
 */
object vorls7 {

  type ~>[A,B] = PartialFunction[A,B] // Folien: type ~>[-A,+B] = PartialFunction[A,B] // extwas vom Typ A oder spezieller und erzeugt etwas vom Type B oder allgemeiner.
  type ->[A,B] = Function[A,B]  // Haskell like signatures
  type ->:[A,B] = Function[A,B]

  val f1:PartialFunction[AnyVal, String] = { case _:Int => "Int" }
  val f2:AnyVal ~> String = { case _:Int => "Int" }

  val add:Int -> (Int -> Int) =
    x => y => x + y // Haskell Type signs. However, its left associative.

  val mult:Int ->: Int ->: Int =
    x => y => x * y  // now with correct associativity.

  // val f: Int ~> Int = (x:Int) => x + 1

  // tailrekursive Funktion.
  // Wenn die Funktion nur einen rekursiven Aufruf Verarbeitung des Ergebnises passiert.
  // Genausoschnell wie der beste imperative Code.

  // Der TCO Test:
  def tco:Int = if(true) tco else 1

  // Wenn diese Funktion in eine TR umgewandelt wurde, dann wird der Stack nicht sofort voll,
  // sondern geht in einen einfache Endlosschleife.

  import scala.annotation.tailrec
  @ tailrec
  def plus(x:Int, y:Int):Int = if (x==0) y else plus(x-1, y+1)

  // Access-Modifiers


}
