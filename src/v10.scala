/**
 * Created by Swaneet on 20.05.2014.
 */
object v10 {

  // Vererbungshierarchie unabhängige operationen refaktorisieren

  // z.B. Type-Class für serialize()/deserialize()

  // Die Idee ist von tiefen Verarbungshierarchien wegzukommen

  // Functor, Monad

  // Futures verwenden Monad-for um die Funktionen zu komponieren.
  // Ganz am Ende holt man das Ergebnis raus

  // Parallel Collecitons
  // collection.parallel enthält Traits
  // collection.parallel.immutable und _.mutable enthält Implementierungen
  // (Gutes Beispiel von Guten Hierarchien. Nur die Blätter der Hieerarchien sind Klassen und Implementierungen,
  //  Alles darüber sind nur traits und abstrakt.)

  // ParVector/ ParSet

  // (0 to 100).par.foldLeft(0)(_+_)

  lazy val s = (0 to 10000).par.foldLeft(0)(_+_)

  def mutableSum() = {
    val arr = (0 to 100).toArray.par
    var sum=0
    arr.foreach(sum+=_) // operation ist nicht atomar. f x s = set (get s + x)
    sum // das Ergebnis ist unterschiedlich.... data races
  }

  def seqSum() = {
    val arr = (0 to 100).toArray
    var sum=0
    arr.foreach(sum+=_) // operation ist nicht atomar. f x s = set (get s + x)
    sum // das Ergebnis ist unterschiedlich.... data races
  }

  def prints() = {
    for(i <- (0 to 10).par) print(i + " ")
    println()
    for(i <- (0 to 10).par) print(i + " ")
    println()
    for(i <- (0 to 10).par) print(i + " ")
    println()
    for(i <- (0 to 10).par) print(i + " ")
  }

  def nonassocReduce() = {
    val arr=(0 to 1000).toArray.par
    println(arr.reduce(_ - _))
    println(arr.reduce(_ - _))
    println(arr.reduce(_ - _))
    println(arr.reduce(_ - _))
  }

  def nonassocFold() = {
    val arr=(0 to 1000).toArray.par
    println(arr.foldLeft(0)(_ - _))
    println(arr.foldLeft(0)(_ - _))
    println(arr.foldLeft(0)(_ - _))
    println(arr.foldLeft(0)(_ - _))
  }

  // wichtig bei parallelen Operationen
  // Assoziativ-Gesetz bei reduce muss gelten.
  // pure Funktionen (Seiteneffektfreie Programmierung)

  def measure[A](x: => A):Unit = {
    import System._
    val t1 = nanoTime()
    x
    val t2 = nanoTime() - t1
    println("<" + (t2/(BigInt(10)^6).toLong) + "ms >")
  }

  def parPrint2 = {
    val parArr = (0 to 10000000).par
    measure(for (i <- parArr) print(if(i%1000000==0) i + " " else ""))
  }

  def seqPrint2 = {
    val arr = (0 to 10000000)
    measure(for (i <- arr) print(if(i%1000000==0) i + " " else ""))
  }
}
