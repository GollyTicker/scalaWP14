/**
 * Created by Swaneet on 30.04.2014.
 */

import U.undefined // für alle Scopes in diesem Modul bereitstellen
object U {
  def undefined = throw new RuntimeException
}

// Zeit unterstüzt Addition, die Rückgabe der Nanosekunden sowie das Überprüfen auf NaTime
sealed trait Time {
  def ns: Long
  def +(t: Time): Time
  def isNaTime: Boolean
}

// Konkrete Time in nanosekunden
class TimeNS(val l: Long) extends Time {
  lazy val ns = l

  import Time.NaTime

  override def +(t: Time): Time = t match {
    case NaTime => NaTime
    case _ => Time(ns + t.ns)
  }

  override def isNaTime = false

  override def equals(o: Any) = {
    lazy val t = o.asInstanceOf[Time];
    o.isInstanceOf[Time] && ns == t.ns
  }

  override def toString = ns + " nsec"
}

object Time {
  // nur positive nanosekunden und 0 sind erlaubt
  def apply(l: Long): Time = if (l >= 0) new TimeNS(l) else NaTime

  def Σ(ts: Time*): Time = ts.foldLeft(Time(0))(_ + _)

  // Konversionen
  private lazy val K = 1000L
  implicit class FancyNum(l: Long) {
    lazy val sec: Time = (l * K).msec
    lazy val msec: Time = (l * K).μsec
    lazy val μsec: Time = (l * K).nsec
    lazy val nsec: Time = Time(l)
  }

  // das Not A Time singleton
  object NaTime extends Time {
    override lazy val ns = undefined
    override def +(t: Time): Time = NaTime
    override lazy val isNaTime = true

    override def toString = "NaTime"
    override def equals(x: Any): Boolean = false
  }

  /*object TimeImplicits {
    implicit def long2Time(l: Long): Time = Time(l)
    implicit def int2Time(i: Int): Time = Time(i.toLong)
  }*/

}

object Ex04 {
  def test {
    import Time._

    println("10 sec: " + 10.sec)
    println("20 msec: " + 20.msec)
    println("10020 msec: " + (10.sec + 20.msec))
    println("10020 msec: " + (20.msec + 10.sec))
    println("4003020051 nsec: " + (1.nsec + 50.nsec + 20.μsec + 3.msec + 4.sec))
    println("4003020051 nsec: " + (3.msec + 4.sec + 1.nsec + 20.μsec + 50.nsec))
    println("4003020051 nsec: " + Σ(3.msec, 4.sec, 1.nsec, 20.μsec, 50.nsec))

    // 10.sec.sec //<- compiliert nicht
    // 10.sec + 10 //<- compiliert nicht

    println("NaTime: " + (-1).sec)
    println("NaTime: " + Σ(3.msec, 4.sec, 1.nsec, NaTime, 20.μsec, 50.nsec))
  }
}