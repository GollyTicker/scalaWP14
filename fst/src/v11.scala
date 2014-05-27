/**
 * Created by Swaneet on 27.05.2014.
 */

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util.Failure

object v11 {

  // weiter Futures
  // onComplete, onSuccess, onFailure (Seiteneffektvolle Run Funktionen)

  def apply(): Unit = {

    val fut = Future({
      1 + 2
    })
    println("Future:" + fut)

    val res = fut.onSuccess({
      case 2 => println("Got: 2!")
      case x: Int => println("Simply: " + x)
    })

    val res2 = Future {
      1 / 0
    } onFailure ({
      case x: Exception => println(x)
    })

    println("Future:" + fut)

    print("0.0/0.0: ")
    Future {
      0.0 / 0.0
    }.onComplete({
      case Failure(err) => println("Err:" + err)
      case res => println("Success:" + res.get) // <-
      //case Success(res) => println("Success:" + res) // <-
    }
    )
    // leider funktioniert das pattern Matching mit success in Idea nicht....
    // aber in repl funzt es.


    print("0/0: ")
    Future {
      0 / 0
    }.onComplete({
      case Failure(err) => println("Err:" + err) // <-
      case res => println("Success:" + res.get)
    })


    // Monadices Komponieren
    val f: Future[Int] = Future {
      3
    }
    val g: Future[Int] = Future {
      5
    }

    val h: Future[Int] = for {
      x: Int <- f
      y: Int <- g
    } yield (x + y)

    println("h: " + h.value) // geht schwer in IDE

    // aber in REPL (zum Kopieren)
    def a = {
      import scala.concurrent.Future
      import scala.concurrent.ExecutionContext.Implicits.global
      val f: Future[Int] = Future {
        3
      }
      val g: Future[Int] = Future {
        5
      }


      val h = for {x <- f; y <- g} yield x + y
      h.value

      // val h = for {x <- Future {2}; y <- Future {3}} yield x + y // Achtung! Blockierendes Future!
      // Berechnung von 3 fängt erst NACH der Fertigstellugn von 2 an!
      // im ersten Fall landen die Futures schon beim "val f = ..." in dem Threadpool und werden berechnet
    }

    // type safe printf
    val sdf = 2332423
    println(s"Hello: sdf")



    // AKTOREN MODELL -- AKTOREN MODELL -- AKTOREN MODELL -- AKTOREN MODELL

    // ScaleUp -> Futures machen Programme mit Prozesse skalierbar
    // ScaleOut -> Aktoren machen RPogramme mit mehreren Prozessen fehlsicher

    // Aktor Modell
    // funktional, value-oriented, MultiCores, distributiv

    // scalesUp, scalesOut
    // eingebaute Fehlertoleranz (self-healing)
    // transparentes Load Balancing

    // Strong isolation
      // processe teilen keinen State
      // processes are units of error encapsulation
      // processe fail as soon as processes
      // error in a process dont affect another procress
      // errors in a process an be detected my other processes

    // 99.999999999% Availability per year => 31ms / year

    // Actor vs. Object
      // siehe Folien
      // Wegen ihrer Speicherung im Heap/Stack und Referenzierung mit Pointer
      // lassen sich Objekte nicht verschieben.
    // Actor ->
    // lock free concurrency
    // asynchronous messaging
    // share no states

    // Tree of Actors

    // 1: Supervisors
    // 2: Supervisors - 2
    // ...
    // n-1: Workers & Supervisors
    // n: Workers


    // Vorteile:
    // Actoren sind bei default nicht blockierend
    // können zwishcen Threads und CPUs wandernd (weil keine Speicherreferenz)
    // Referenzen auf Actors sind nicht-place oriented

    // Merkmale
    // Jeder angeforderte Servie resultiert in einen weiteren Message als Antwort zurück
    // (oder eine Weiterleitung des Ergebnisses an ein anderes Objekt) (z.B. A fragt B. B schickt Antwort zu C.)

    // Nachricht
      // Dispatcher ( Welcher Handler soll für diese Message verwendet werden? )
      // Message wird aus Mailbox gelöscht


  }

}
