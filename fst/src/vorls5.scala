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
  // "Ich mahc ediesen Type jetzt zu Eis. Der Typ kann nicht mehr Extended werden. Es entsteht ein ADT."

  sealed trait Tree
  case class Leaf(value:Int) extends Tree
  case class Node(left:Tree, right:Tree) extends Tree

  // Beispiel patternmatching
  def exists(t:Tree, i:Int):Boolean = t match {
    case Leaf(value) => value == i
    case Node(l, r) => exists(l, i) || exists(r, i)
  }

  def exists2(t:Tree, i:Int):Boolean = t match {
    case Node(l, r) => exists(l, i) || exists(r, i)
    case Leaf(`i`) => true // die backticks sagen dem compiler aus dem Inhalt keine lokale variable zu machen sondenr eine bestehende zu verwenden.
    case _ => false
  }

  // Referenz zum Objekt behalten mit  @ Syntax. (aus haskell)


  val tree1 =
    Node(
      Node(
        Leaf(-1),
        Node(
          Leaf(100),
          Leaf(10)
        )
      ),
      Node(
        Leaf(0),
        Leaf(5)
      )
    )

  var infTree:Tree = Node(infTree, infTree)
  val infListTree:Tree = Node(Leaf(1), infListTree)
  // recursion doesnt work immediately :/

  def printTrees = {
    println(Node(Leaf(5), Node(Leaf(3),Leaf(4)) ))
    println(tree1)
    println(infTree)
    println(infListTree)

    println(exists(tree1, 0))
    println(exists(tree1, 5))
    println(exists(tree1, 1))
  }


  // Strictness vs Non-Strictness
  // Strict bzw. eager
  // oder
  // lazy bzw. non-strict

  // Non Strict. Die Argumente werden erst zum Gebrauch des wertes berechnet.
  // Bei nicht-puren Funktionen macht aber die verkehrte Ausführungsreihenfolge
  // der Argumente zum Funktionenkörper einen Unterschied. Dies führt zum Nicht-Deterministischen Verhalten.
  // Gibt es in Java als Short-Circuting bei logischen Operatoren.
          // s1 ist strict(call-by-value). // s2 ist non-strict(call-by-name).
  def nonStrict(s1:String, s2: => String) = print(if (s1 != "") s1 else s2 + " " + s2)

  def strictRun(){
    nonStrict( {print(1); "Hallo"}, {print(2); "Welt!"})   // => 1Hallo
    println("")
    nonStrict( {print(1); ""}, {print(2); "Welt!"})   // => 122Welt! Welt!
    // der Block muss für die Stringkonkatenation zweimal berechnet werden, da der Wert nicht gespeichert wird.

    // val und var werden direkt zur Anlage strict berechnet.
    // Methoden und Funktionen werden immer non-strict brechnet. (dh erst buej Aufruf.)


  }


}
