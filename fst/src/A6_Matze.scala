/**
 * Created by sacry on 24/04/14.
 */

object ForEach {
  def forEach[A](f: (Int => Int => A), n: Int): List[A] = {
    (for (i <- (0 until n); j <- (0 until n)) yield f(i)(j)).toList
  }
}

class Matrix private(matrix: Array[Double], val n: Int) {

  import ForEach.forEach

  def apply(i: Int, j: Int) = matrix(i * n + j)

  lazy val size = n * n

  def sameSizeOf(m: Matrix) = m.size == size

  def +(m1: Matrix) = {
    require(sameSizeOf(m1))
    new Matrix(forEach(i => j => m1(i, j) + this(i, j), n).toArray, n)
  }

  def *(x: Double) = new Matrix(forEach(i => j => x * this(i, j), n).toArray, n)

  // https://en.wikipedia.org/wiki/Matrix_multiplication#Matrix_product_.28two_matrices.29
  // (A*B)(i)(j) == sum {k, 1, m, A(i)(k) * B(k)(j)
  def *(other: Matrix) = {
    require(sameSizeOf(other))
    new Matrix(
      forEach( i => j => (0 to n).map(k => this(i,k) * other(k,j) ).sum, n).toArray
      , n)
  }

  def -(m1: Matrix) = this + (- m1)

  def unary_- = new Matrix(forEach(i => j => -this(i, j), n).toArray, n)

  override def equals(any: Any) = {
    lazy val other = any.asInstanceOf[Matrix]
    any.isInstanceOf[Matrix] && sameSizeOf(other) &&
      forEach(i => j => other(i, j) == this(i, j), n).reduceLeft(_ && _)
  }

  override def toString = {
    ("-" * 40) + "\n" + matrix.sliding(n, n).map(_.mkString(", ")).mkString("\n")
  }

}

object Matrix {

  import ForEach.forEach

  def apply(x: Double*) = {
    if (math.sqrt(x.size).isWhole)
      new Matrix(x.toArray, math.sqrt(x.size).toInt)
    else
      throw new RuntimeException("DIE!")
  }

  def apply(xs: Array[Array[Double]]) =
    if (xs.forall(x => x.size == xs.size))
      new Matrix(xs.foldLeft(Array[Double]())((acc, elem) => acc ++ elem), xs.size)
    else
      throw new RuntimeException("DIE!")

  def apply(n: Int, optional: Double = 0.0) = new Matrix(
    forEach(i => j => optional, n).toArray, n
  )

  def apply(n: Int, f: ((Double, Double) => Double)) = new Matrix(
    forEach(i => j => f(i, j), n).toArray, n
  )

  def apply(n: Int, t: Triple[Double, Double, Double]*) =
    new Matrix(
      (for (i <- (0 until n);
            j <- (0 until n);
            (i1, j1, k) <- t;
            if i == i1 && j1 == j)
      yield k).toArray, n
    )

  def Zero(n:Int) = Matrix(n, 0.0)
  def Unit(n:Int) = Matrix(n, (i, j) => if (i == j) 1.0 else 0.0)

  def main(args: Array[String]) {
    val mat1 = Matrix(1.0, 2.0, 3.0, 4, 5, 6, 7, 8, 9)
    val mat2 = Matrix(3, (i, j) => if (i == j) 1.0 else 0.0)
    val mat3 = Matrix(Array(Array(3.0, 2.0, 1.0), Array(0.0, 1, 0), Array(0.0, 0, 3)))
    val mat4 = Matrix(5, 1.0)
    val mat5 = Matrix(2, (0.0, 0.0, 10.0), (0.0, 1.0, 10.0), (1.0, 0.0, 10.0), (1.0, 1.0, 10.0))
    println(mat2)
    println(mat3)
    println(mat2 + mat3)
    println("Eq self: " + (mat1 == mat1) )
    println("Eq noit self: " + (mat1 != mat2) )
    println("Eq not null " + (mat1 != null) )
    println("Mult: " + mat4*(mat4*0.3) )

    println("Mult id" + (mat5*Unit(mat5.n) == mat5) )
    //println("Mult anhi" + (mat5*Zero(mat5.n) == Zero(mat5.n)) )
    //println("Mult Asso:" + ((mat5*mat3)*mat4 == mat5*(mat3*mat4)) )
  }

}