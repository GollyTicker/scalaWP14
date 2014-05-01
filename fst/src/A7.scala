/**
 * Created by Swaneet on 30.04.2014.
 */

import U.undefined

object U {
  def undefined = throw new RuntimeException
}

// insteadt of simply saving the number as nsec, μsec, sec etc, I'm saving
// the decimal digit
case class Time private(val ns: => Long, val isNaTime: => Boolean) {

  def +(t: Time): Time = new Time(ns + t.ns, isNaTime || t.isNaTime)

  override def equals(o: Any) = {
    lazy val t = o.asInstanceOf[Time]; o.isInstanceOf[Time] && ns == t.ns
  }

  // override def toString() = undefined
}

object Time {
  lazy val NaTime: Time = Time(undefined, true)

  def Σ(ts: Time*): Time = ts.sum

  implicit class FancyNum(l: Long) {
    lazy val sec: Time = undefined
    lazy val msec: Time = undefined
    lazy val nsec: Time = undefined
    lazy val μsec: Time = undefined
  }

}

object TimeFactory

object Ex04 {

  import TimeImplicits._

  def test {
    import Time._
    println(10.sec)
    println(20.msec)
    println(10.sec + 20.msec)
    println(20.msec + 10.sec)
    println(1.nsec + 50.nsec + 20.μsec + 3.msec + 4.sec)
    println(3.msec + 4.sec + 1.nsec + 20.μsec + 50.nsec)
    println(Σ(3.msec, 4.sec, 1.nsec, 20.μsec, 50.nsec))
    // 10.sec.sec //<- compiliert nicht
    // 10.sec + 10 //<- compiliert nicht
    println((-1).sec)
    println(Σ(3.msec, 4.sec, 1.nsec, NaTime, 20.μsec, 50.nsec))
  }
}

object TimeImplicits {
  implicit def long2Time(l: Long): Time = undefined
  implicit def int2Time(i: Int): Time = undefined
}