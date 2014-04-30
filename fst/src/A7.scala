/**
 * Created by Swaneet on 30.04.2014.
 */
import U.undefined
object U {def undefined = throw new RuntimeException}

case class Time private (val d: => Int, val exp: => Int, val valid: => Boolean) {
  lazy val sec:Time = undefined
  lazy val msec:Time = undefined
  lazy val nsec:Time = undefined
  lazy val μsec:Time = undefined

  def +(t:Time):Time = undefined

  override def equals(o:Any) = { lazy val t = o.asInstanceOf[Time]; o.isInstanceOf[Time] && d==t.d && exp == t.exp}
}

object Time {
  lazy val NaTime:Time = Time(undefined, undefined, false)
  def Σ(ts:Time*):Time = undefined
}

object TimeFactory {
  import math.log10
  def apply(l:Long):Time = new Time(l.toInt%10, log10(l.toDouble).floor.+(1).toInt, true)
  def apply(i:Int):Time = TimeFactory(i.toLong)
}


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
    10.sec.sec //<- compiliert nicht
    10.sec+10 //<- compiliert nicht
    println((-1).sec)
    println(Σ(3.msec, 4.sec, 1.nsec, NaTime, 20.μsec, 50.nsec))
  }
}

object TimeImplicits {
  implicit def long2Time(l: Long):Time = TimeFactory(l)
  implicit def int2Time(i: Int):Time = TimeFactory(i)
}