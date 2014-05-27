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



  }

}
