/**
 * Created by Swaneet on 02.06.2014.
 */

import scala.concurrent.{ExecutionContext, duration}
import duration._
import ExecutionContext.Implicits.global
import scala.concurrent._
import myUtils.ExecTiming._

object A11 {

  def apply() = {
    val m0 = IMatrix(10, (i, j) => if (i == j) i + 1 else 0)
    val m3 = IMatrix(1000, (i, j) => i + j)
    printResultAndMillis(m0 * m0)
    printMillis(m3 * m3)
  }

}

class IMatrix(val dim: Int, private val arr: Array[Int]) {
  def apply(i: Int, j: Int) = if (0 <= i && i < dim && 0 <= j && j < dim) Some(arr(i * dim + j)) else None

  val get: (Int, Int) => Int =
    (i, j) => apply(i, j).get

  private val set: (Int, Int) => Array[Int] => Int => Unit =
    (i, j) => arr => v => {
      arr.update(dim * i + j, v); ()
    }

  def *(that: IMatrix): IMatrix = {
    def calcPartition(from: Int, to: Int, res: Array[Int]): Int = {
      var n = 0 // nur zur Kontrolle, nicht relevant für Multiplikation
      for (i <- from until to)
        for (j <- 0 until dim) {
          var sum = 0
          for (k <- 0 until dim)
            sum += arr(i * dim + k) * that.arr(k * dim + j)
          res(i * dim + j) = sum
          n += 1
        }
      n
    }


    val res = Array.fill(dim * dim)(0)



    def calcElem(i: Int, j: Int) = {
      set(i, j)(res) {
        (0 until dim).map(k => get(i, k) * get(k, j)).sum
      }
    }

    // (A*B)(i)(j) == sum {k, 1, n, A(i)(k) * B(k)(j)
    for {
      i <- 0 until dim
      j <- 0 until dim
      calcSum = calcElem(i, j)
    } yield ()


    new IMatrix(dim, res)
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
