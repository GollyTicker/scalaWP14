/**
 * Created by Swaneet on 15.04.2014.
 */
object vorls5 {

  // ADT- Pattern matching
  // Eine Abstrakter Datentype der eine FIXE ANZAHL von Konstruktoren. Evtl. inverse Operationen dazu.

  /*
  * element match {
  *   case pattern1 => exp1
  *   case pattern2 if cond => exp2
  *   case pattern3:Type => exp3
  *   case _ => defaultExp
  * }
  */
  // Fällt ein Objekt durch alle Patterns durch, dann gibts es eine MatchExcpetion!

  // Ideal für Case Klassen. Sie sind von vorn herein ADT.
  // case KLassen bekommen automatisch die Vorbereitungen für Pattern Matchings.


  // Essen: Verifizieren und Testen.

  def fizzbuzz(i:Int) = i match {
    case _ if i % 15 == 0 => "FizzBuzz"
    case _ if i % 3 == 0 => "Fizz"
    case _ if i % 5 == 0 => "Buzz"
    case _ => i
  }

  def printFizzBuzz = for (i <- 1 to 50) {
    print(fizzbuzz(i) + " ")
  }



  // sealed traits

}
