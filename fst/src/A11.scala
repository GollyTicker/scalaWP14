/**
 * Created by Swaneet on 02.06.2014.
 */

import scala.concurrent.{ExecutionContext, duration}
import duration._
import ExecutionContext.Implicits.global
import scala.concurrent._
import myUtils.ExecTiming._
import myUtils.await

object A11 {

  def apply(x:Int = 1000) = {
    val m0 = IMatrix(6, (i, j) => if (i == j) i + 1 else 0)
    val m3 = IMatrix(x, (i, j) => i + j)  // 500 -> 15sec, 250 -> 2.2ec, 100 -> 0.1sec

    printResultAndMillis(m0 * m0)

    val slow1 = millis(m0 slowMult m0)
    val slow2 = millis(m3 slowMult m3)

    val fast1 = millis(m0 * m0)
    val fast2 = millis(m3 * m3)

    println("slow vs. fast")
    println(slow1 + " vs. " + fast1)
    println(slow2 + " vs. " + fast2)
  }

}

class IMatrix(val dim: Int, private val arr: Array[Int]) {
  def apply(i: Int, j: Int) = if (0 <= i && i < dim && 0 <= j && j < dim) Some(arr(i * dim + j)) else None

  val get: (Int, Int) => Int =
    (i, j) => apply(i, j).get

  // (A*B)(i)(j) == sum {k, 1, n, A(i)(k) * B(k)(j)
  def slowMult(that:IMatrix):IMatrix = {
    val res = Array.fill(dim * dim)(0)
    val set: (Int, Int) => Array[Int] => Int => Unit = (i, j) => arr => v => { arr.update(dim * i + j, v); () }
    def calcElem(i: Int, j: Int) = set(i, j)(res) ( (0 until dim).map(k => get(i, k) * get(k, j)).sum )
    for { i <- 0 until dim; j <- 0 until dim; a = calcElem(i, j) } yield ()
    new IMatrix(dim, res)
  }

  def *(that: IMatrix): IMatrix = {
    val res = Array.fill(dim * dim)(0)
    val set: (Int, Int) => Array[Int] => Int => Unit = (i, j) => arr => v => { arr.update(dim * i + j, v); () }

    def calcElem(i: Int, j: Int) = {
      Future(set(i, j)(res) {
        (0 until dim).map(k => arr(i *dim +  k) * that.get(k, j)).sum
      })
    }
    // (A*B)(i)(j) == sum {k, 1, n, A(i)(k) * B(k)(j)
    val workers = for {
      i <- 0 until dim
      j <- 0 until dim
    } yield calcElem(i, j)

    workers.foreach( f => Await.ready(f,Duration.Inf) )
    val ret = new IMatrix(dim, res)
    println("should not be 0:" + ret(dim-1,dim-1))
    ret
  }

  override def toString = {
    val srows = for (r <- arr.sliding(dim, dim)) yield r.mkString("(", ",", ")\n")
    "Matrix(\n" + srows.foldLeft("")((s, r) => s + r) + ")"
  }
}

object IMatrix {
  def apply[N](dim: Int, f: (Int, Int) => Int) = {
    val vec = Array.fill(dim * dim)(0)
    for (i <- 0 until dim; j <- 0 until dim)
      vec(i * dim + j) = f(i, j)
    new IMatrix(dim, vec)
  }
}
