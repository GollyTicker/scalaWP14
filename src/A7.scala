/**
 * Created by Swaneet on 30.04.2014.
 */

import U.undefined // für alle Scopes in diesem Modul bereitstellen
object U {
  def undefined = throw new RuntimeException
}

object MyTime {
  // Zeit unterstüzt Addition, die Rückgabe der Nanosekunden sowie das Überprüfen auf NaTime
  trait Time {
    def ns: Long
    def +(t: Time): Time
    def *(l:Long): Time
    def isNaTime: Boolean
  }
  // Konversionen auf Long. public
  private lazy val K = 1000L
  implicit class FancyNum(l: Long) {
    lazy val sec: Time = l.msec*K
    lazy val msec: Time = l.μsec*K
    lazy val μsec: Time = l.nsec*K
    lazy val nsec: Time = Time(l)
  }

  // das Not A Time singleton
  object NaTime extends Time {
    override lazy val ns = undefined
    override def +(t: Time): Time = NaTime
    override lazy val isNaTime = true
    override def *(l:Long) = NaTime

    override def toString = "NaTime"
    override def equals(x: Any): Boolean = false
  }

  // Konkrete Time in nanosekunden
  class TimeNS(val l: Long) extends Time {
    lazy val ns = l

    override def +(t: Time): Time = if (t.isNaTime) NaTime else Time(ns + t.ns)

    override def *(l:Long):Time = Time(ns*l)

    override def isNaTime = false

    override def equals(o: Any) = {lazy val t = o.asInstanceOf[Time]; o.isInstanceOf[Time] && !t.isNaTime && ns == t.ns}

    override def toString = {
      val sec_cut:Long  = ns/K/K/K  // get rid of trailing numbers
      val msec_cut:Long = ns/K/K
      val μsec_cut:Long = ns/K
      () match {  // if the lost digits were zeros, then this equality holds.
        case _ if sec_cut*K*K*K  == ns  => sec_cut  + " sec"
        case _ if msec_cut*K*K   == ns  => msec_cut + " msec"
        case _ if μsec_cut*K     == ns  => μsec_cut + " μsec"
        case _                          => ns       + " nsec"
      }
    }
  }
  // re-export for visibility
  def Σ(ts:Time*):Time = Time.Σ(ts)

  object Time {
    // Only positive and zero Nanoseconds are valid.
    private[MyTime] def apply(ns: Long): Time = if (ns >= 0) new TimeNS(ns) else NaTime
    def Σ(ts: Seq[Time]): Time = ts.foldLeft(Time(0))(_ + _)
  }
}



object Ex04 {
  def test {
    import MyTime._

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