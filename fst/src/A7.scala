/**
 * Created by Swaneet on 30.04.2014.
 */
import U.undefined
object U {def undefined = throw new RuntimeException}

// insteadt of simply saving the number as nsec, μsec, sec etc, I'm saving
// the decimal digit
case class Time private (val l: => Long, val isNaTime: => Boolean) {
  lazy val sec:Time = undefined
  lazy val msec:Time = undefined
  lazy val nsec:Time = undefined
  lazy val μsec:Time = undefined

  def +(t:Time):Time = new Time(l + t.l, isNaTime || t.isNaTime)

  override def equals(o:Any) = { lazy val t = o.asInstanceOf[Time]; o.isInstanceOf[Time] && nsec == t.nsec}
  // override
}

object Time {
  lazy val NaTime:Time = Time(undefined, true)
  def Σ(ts:Time*):Time = ts.sum
}

object TimeFactory {
  import math.log10
  def apply(l:Long):Time = new Time(l, false)
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