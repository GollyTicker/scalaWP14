/**
 * Created by Swaneet on 28.04.2014.
 */
object A6 {
  import Stream._

  lazy val withFrom:Stream[Int] = from(1).map(triExp)

  def triExp(n:Int) = n*(n+1)/2

  def triInd(n:Int):Int = {
    import math.sqrt
    lazy val oldN:Int = findNfor(n * 2)
    def findNfor(d:Int) = {
      from(0).takeWhile( _ <= sqrt(d)).flatMap( n => if (n*(n+1) == d) n::Nil else Nil ).head
    }
    (oldN + 1)*(oldN + 2)/2
  }

  def testStreams[A](st1:Stream[A], st2:Stream[A]):Boolean = st1.take(14) == st2.take(14)

  lazy val withConsFirst:Stream[Int] = {
    def next(n:Int):Stream[Int] =  Stream.cons(triExp(n), next(n + 1))
    next(1)
  }

  lazy val withIterate:Stream[Int] = Stream.iterate(1)(triInd)

  lazy val withConsSecond:Stream[Int] = cons(1, withConsSecond.map(triInd) )

  lazy val triangleResult = Stream(1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66, 78, 91, 105)

  // hdStreams [] = []
  // hdStreams (x:xs) = map (x:) ([]:hdStreams(xs))
  def headStreams[A](st:Stream[A]):Stream[Stream[A]] = st match {
    case Empty => Empty
    case hd #:: tl => (Empty #:: headStreams(tl)).map( hd #:: _ )
  }
  lazy val exampleHeadStreams = headStreams(from(10).take(5))


  def run() = {
    // println(exampleHeadStreams.map(_.force).force)
    println(exampleHeadStreams == Stream(Stream(10), Stream(10, 11), Stream(10, 11, 12), Stream(10, 11, 12, 13), Stream(10, 11, 12, 13, 14)))

    println(testStreams(withFrom, withConsFirst))
    println(testStreams(withFrom, withIterate))

    //println(withConsSecond.take(14).force)

    println(testStreams(withFrom, withConsSecond))

  }

}
