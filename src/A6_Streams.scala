/**
 * Created by Swaneet on 28.04.2014.
 */
import Stream._

object A6 {
  lazy val withFrom:Stream[Int] = from(1).map(triExp)

  def triExp(n:Int) = n*(n+1)/2

  def triInd(n:Int):Int = {
    import math.sqrt
    lazy val oldN:Int = findNfor(n * 2)
    lazy val lowerBound:Int = sqrt(n).floor.asInstanceOf[Int]
    def findNfor(d:Int) = {
      from(lowerBound).takeWhile( _ <= sqrt(d)).flatMap( n => if (n*(n+1) == d) n::Nil else Nil ).head
    }
    (oldN + 1)*(oldN + 2)/2
  }

  def testStreams[A](st1:Stream[A], st2:Stream[A]):Boolean = st1.take(14) == st2.take(14)

  lazy val withNext:Stream[Int] = {
    def next(n:Int):Stream[Int] =  Stream.cons(triExp(n), next(n + 1))
    next(1)
  }

  lazy val withIterate:Stream[Int] = Stream.iterate(1)(triInd)

  lazy val withCons:Stream[Int] = cons(1, withCons.map(triInd) )

  lazy val withOperator:Stream[Int] = 1 #:: 3 #:: withCons.zip(withCons.tail).map { case (n1, n2) => n2 + (n2 - n1) + 1 }

  val triangleResult = Stream(1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66, 78, 91, 105).force

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

    println(testStreams(triangleResult, withFrom))
    println(testStreams(triangleResult, withNext))
    println(testStreams(triangleResult, withIterate))

    //println(withCons.take(14).force)

    println(testStreams(triangleResult, withCons))
    println(testStreams(triangleResult, withOperator))
  }

}
