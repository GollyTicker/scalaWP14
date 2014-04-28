/**
 * Created by Swaneet on 28.04.2014.
 */
object A6 {
  import Stream._

  lazy val withFrom:Stream[Int] = from(1).map(n => n*(n+1)/2)

  def testStreams[A](st1:Stream[A], st2:Stream[A]):Boolean = st1.take(14) == st2.take(14)

  lazy val withCons:Stream[Int] = throw new RuntimeException

  def headStreams[A](st:Stream[A]):Stream[Stream[A]] = st match {
    case Empty => Empty
    case hd #:: tl => cons(hd, Empty) #:: headStreams(tl).map( hd #:: _ )
  }
  lazy val exampleHeadStreams = headStreams(from(10).take(5))

  def run() = {
    println(exampleHeadStreams.map(_.force).force)
    println(exampleHeadStreams == Stream(Stream(10), Stream(10, 11), Stream(10, 11, 12), Stream(10, 11, 12, 13), Stream(10, 11, 12, 13, 14)))
  }

}
