/**
 * Created by Swaneet on 22.04.2014.
 */
object vorls6 {
  def fib(n:Long):Long = if (n <= 0) 0 else (if (n==1) 1 else fib(n-1) + fib(n-2))

  // lazy argument
  // zusammen mit lazy val
  def mSqr(n: => Long):Long = {
    lazy val x = n
    x*x// erst hier wird das val berechnet und zweimal verwendet
  }

  // objects sind Module mit Klassen, Feldern und weiteren Objekten und können
  // als Argumente weitergereicht werden.

  // Identifier und Operatoren
  // Prefix sind 4 viererlaubt: + - ! ~
  // z.B. +4 oder ~2
  // definition mit z.B. "def unary_+" für +
  // Preäzedenz bei Infixoperatoren nur vom ersten OPerator abhängig.
  // lowest | ^ &   < >   = !   : + -    * / % (andere Operatorensymbole) highest
  // Ausnahemen sind die Zuweisungsoperatoren:
  // = += -= *= /*

  // Assoziativ:
  // Infixoperator ist recht-assoziativ, falls das letzte Zeichen ein : ist.
  // 1 :: Nil
  // wird zu
  // Nil.::(1)

  val caseFunction:AnyVal => String = {
    case 10.0 => "10.0"
    case x:Byte => "Byte"
    case _ => "anything else"
  }
}

// lazy Object.
// Singleton Objekte werden erst beim ersten Zugriff berchnet.


// normal(nicht lazy)
object Eins
object Zwei extends AnyRef // das gelcihe wie oben. (heißt soviel wie extends Object

trait T {
  def mult(j:Int):Double
}

class D(val d:Double)

// ein object ist eine singleton Instance der dazugehörigen Anynymen klasse
object O3 extends D(10.0) with T {
  def mult(j:Int) = d*j
}

// scala> O3.mult(5)

// / jetzt mit lazyness
object O4 {
  class C {def foo="foo"}
}
// scala> new (O4.C).foo

class DC(d:Double) {
  println("DC:("+d+")")
}
case object Obj extends DC(2) // case Object ist ein "lazy objekt"
// beim Lesen dieser Definition wird das singleton noch nicht erzeugt.

// scala> val b =DC(1)  // DC(1.0)
// scala> Obj   // DC(2.0) // Obj


// Scala hat Streams (lazy lsiten aus Haskell). Die Elemente werden erst bei Evaluierung berechnet.
// potentiell unendlich viele Elemente.

// Streams werden mit einem Head(erstes Element) mit einem optinalen Tail.
// Der Tail enthält die nachfolgenden Elemente und/oder eine Kostruktionsregel um die nachfolgenden Elemente zu erzeugen.
//  Jedoch muss man bei manchen Collektionsfunktionen wie ymsteram.size aufpassen.
// Jedoch ist toString ist sicher. Es zeigt nur die evaluvierten Elemente an.

// Stream cons wie das Listen cons:
// 1:: Nil
// 1#:: Stream.empty

// case object NaC extends Complex(Double.NaN, Double.NaN)    // IEEE Idiom. Lieber Fehlerzahl statt Exception.

// Closure

// Anonyme case-Funktionen
// Partial Function bei Aktoren als Dispather z.B. sehr wichtig/nützlich

object Streams {
  import scala.Stream._
  val str:Stream[Int] = 1 #:: 3 #:: 2 #:: Stream.empty
  lazy val inf1:Stream[Int] = 1 #:: inf1  // doesnt work.
  def inf2:Stream[Int] = cons(0,cons(1,inf2)) // works!



  import scala.util.Random._
  def randomBins:Stream[Int] = cons(nextInt(2), randomBins)

  val asString:Stream[Int] => String = _.mkString("")
  //scala> Streams.asString(Streams.randomBins.take(15))
  //res0: String = 000110011001110

  //scala> Streams.asString(Streams.randomBins)
  // .....

  // bei "def" wird der stream bei jendem mal berechnet.
  def randomBytes:Stream[Byte] = cons(nextInt(2).toByte, randomBytes)

  def runbinaries = {
    println(randomBins)
    // dieses val hier wird nur 1x berchnet.
    val rb = randomBins.take(34).force
    def newrb = randomBins.take(34).force
    println(rb)
    println(rb)
    println(newrb)
    println(newrb)
  }

  lazy val fibs:Stream[BigInt] = BigInt(0) #:: BigInt(1) #:: fibs.zip(fibs.tail).map( n => {print("For n: " + n);n._1 + n._2})

  // scala> Streams.fibs.take(5)
  // res0: scala.collection.immutable.Stream[BigInt] = Stream(0, ?)

  // scala> Streams.fibs.take(5).force
  // For n: (0,1)For n: (1,1)For n: (1,2)res1: scala.collection.immutable.Stream[BigInt] = Stream(0, 1, 1, 2, 3)

  // scala> Streams.fibs.take(10).force
  // For n: (2,3)For n: (3,5)For n: (5,8)For n: (8,13)For n: (13,21)res2: scala.collection.immutable.Stream[BigInt] = Stream(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)

  // scala> Streams.fibs(1000)  // das kommt schnell!
  // res5: BigInt = 43466557686937456435688527675040625802564660517371780402481729089536555417949051890403879840079255169295922593080322634775209689623239873322471161642996440906533187938298969649928516003704476137795166849228875

  lazy val randomInt:Stream[Int] = nextInt(10) #:: randomInt
  // es wird memoisiert....
  // scala> Streams.randomInt.take(15).force
  // res0: scala.collection.immutable.Stream[Int] = Stream(4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4)

  // dies funktioniert!
  lazy val goodRandomInt:Stream[Int] = {
    def nextI(i:Int):Stream[Int] = nextInt(10) #:: nextI(i + 1)
    nextI(0)
  }

  // dies funktioniert!
  lazy val goodRandomInt3:Stream[Int] = nextInt(10) #:: goodRandomInt3.map( _ => nextInt(10) )



}

class C {
  val i = { println("i");  f}
  lazy val j = {println("j"); f}
  var k = {println("k"); 0}
  def f = {println("f"); k+=1; k} // hier wird der Default wert hochgezählt.... bevor es eigentlich initialisiert wird!
}

/*
scala> val c = new C
i
f
k
c: C = C@5b0046
scala> println(c.i + ", " + c.j+","+c.k+","+c.f)
j
f
f
1, 1,1,2
scala> println(c.i + ", " + c.j+","+c.k+","+c.f)
f
1, 1,2,3
* */