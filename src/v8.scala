/**
 * Created by Swaneet on 06.05.2014.
 */
object v8 {

  // Testing

  // Varifikation (Syntax und Semantik)
  // Semantische Überprüfung annährendweise durch
  // den durch Compiler überprüfte Axiome/Constraints. (siehe QuickCheck)

  // FP-logische Funktionen können aufgrund der

  // Validierung: Testend durch(für den) den Endbenutzer.

  // Verifizierung /= Validierung

  // Fehlerekennung.

  // Compiler-Fehler: GUT
  // Runtimefehler: Unschön, aber durch Tests fangbar
  // Ignore-Fehler: Fehler die selbst zur Laufzeit nicht erkannt werden. (katastrophal!)

  // Verifikation: Constraints/Axiome.
  // Pre-Post Conditions, Invariants, Assertions. (nicht im produktivcode verwenden)

  // Microsoft entwickelt "Dafny"
  // experimentelle Sprache mit denen
  // Kann bereits zur COMPILEZEIT die Axiome überprüfen!!

  // Interpretation von Exceptions als Fehlerwerten statt Kontrollflussänerungen.
  // NaN vs. try-catch


  val f:Int => Nothing = x => throw new RuntimeException

  val e = try { 1/0 } catch { case e:Exception => e}  // Exception als Wert.

  val dList = List(Some(-100), Some(99), None, Some(1), None)

  val played = dList.map( _ map ( math.abs(_)) filter ( _ < 100) )

  def printLs() = { played filter ( !_.isEmpty ) map ( println(_) ); () }
  def printLs2() = { played map ( x => x map { _.toString + "\n" } getOrElse "" ) map ( print(_) ); () }

  val nothing = ???


  // Großes Scala Beispiel. Twitter.
  // Ergebnise ihrer Zusammenarbeit:

  // abstract class Try[+T]
  // Ähnlich wie das Haskell Either
  // Try[+T] is entweder
  // Failure[+T](val exception:Throwable) oder
  // Success[+T](value: T).

  // Fehlerhanlding wird ganz anders, wenn es um Concurrent/Parallel Aufgaben geht. Siehe Aktor, siehe Erlang etc...
  // Error Handling in Aktors is much more developed than in conventional programming.


  // Known Unknowns (ich weiß, dass es unbekannte Fehler gibt.)
  // Unknown Unknowns (Fehlerbehandlung unbekannter Fehler.)

  // synchronized/wait/notify hilft Parallelisierung nicht!
  // synchronized macht atomar und visibility für andere Cores.
  // volatile macht nur sichtbar.


  // Strukturelle vs. Nominale Programmiersprachen

  // Typr refinement ("Anonyme Interfaces")
}
