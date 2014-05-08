import scala.math.Ordering

/**
 * Created by Swaneet on 08.05.2014.
 */
object A8 {

  def sequence[A](ls: List[Option[A]]): Option[List[A]] = ls match {
    case Nil => Some(Nil)
    case mx :: mx_s => for {
      x <- mx
      xs <- sequence[A](mx_s)
    } yield (x :: xs)
  }

  def testSeq() {
    println( sequence(List(Some(1), Some(2), None)) )
    println(None == sequence(List(Some(1), Some(2), None)))

    println( sequence(List(Some("Hallo"), Some("Welt"))) )
    println(Some(List("Hallo", "Welt")) == sequence(List(Some("Hallo"), Some("Welt"))))
  }


  case class Person(name: String) extends Ordered[Person] {
    override def compare(that: Person): Int = name compare (that.name)
  }

  case class Company(p: Person) extends Ordered[Company] {
    override def compare(that: Company): Int = p compare (that.p)
  }

  val customers = List(Left(Person("P1")), Left(Person("P2")), Right(Company(Person("CEO1"))), Left(Person("P3")),
    Right(Company(Person("CEO2"))), Right(Company(Person("CEO3"))), Left(Person("P3")))

  import Function.uncurried

  def sortEither(ls: List[Either[Person, Company]]): List[Either[Person, Company]] = ls.sortWith( (x,y) => cmpEither(x)(y) < 0 )

  val cmpEither: Either[Person, Company] => Either[Person, Company] => Int =
    fst => snd => (fst, snd) match {
        case (Left(x), Left(y)) => x compare y
        case (Right(x), Right(y)) => x compare y
        case (Left(_), _) => -1
        case _ => 1
    }

  val sortAndSplit: List[Either[Person, Company]] => (List[Either[Person, Company]],List[Either[Person, Company]]) =
    ls => {
    val sorted = sortEither(ls)
    (selectPersons(sorted).map( Left(_) ),selectCompanies(sorted).map( Right(_) ))
  }

  val sortResult = (List(Left(Person("P1")), Left(Person("P2")), Left(Person("P3")), Left(Person("P3"))),
    List(Right(Company(Person("CEO1"))), Right(Company(Person("CEO2"))), Right(Company(Person("CEO3")))))

  def selectPersons[A, B](ls: List[Either[A, B]]): List[A] = ls.flatMap {
    case Left(x) => List(x);
    case Right(x) => Nil
  }

  val persons = List(Person("P1"), Person("P2"), Person("P3"), Person("P3"))

  def selectCompanies[A, B](ls: List[Either[A, B]]): List[B] = ls.flatMap {
    case Left(x) => Nil;
    case Right(x) => List(x)
  }

  val companies = List(Company(Person("CEO1")), Company(Person("CEO2")), Company(Person("CEO3")))

  def testEither() {

    println(customers)

    println(sortAndSplit(customers))
    println(sortAndSplit(customers) == sortResult)

    println(selectPersons(customers))
    println(selectPersons(customers) == persons)

    println(selectCompanies(customers))
    println(selectCompanies(customers) == companies)


  }

  trait ~>[A, B] extends (A => B) {
    def applyOrDefault(x: A, default: B): B
  }

  def use1[A, B](f: A => B)(x: A): B = f(x)

  def use2[A, B](f: A ~> B)(x: A)(default: B): B =
    f.applyOrDefault(x, default)

  val f: Double ~> Double = new (Double => Double) with (Double ~> Double) {
    def applyOrDefault(x: Double, default: Double): Double = if (x >= 0) this(x) else default

    def apply(x: Double): Double = math.sqrt(x)
  }

  def testFunction() = {
    println(use1(f)(2))
    println(use1(f)(2) == math.sqrt(2))

    println(use1(f)(-2.0))
    println(use1(f)(-2.0) != Double.NaN) // NaN != NaN => true

    println(use2(f)(0.0)(-2.0))
    println(use2(f)(0.0)(-2.0) == 0.0)
  }


  trait Functor[F[_]] {
    def fmap[A, B](fa: F[A], f: A => B): F[B]
  }

  val listFunctor: Functor[List] = new Functor[List] {
    def fmap[A, B](ls: List[A], f: A => B) = ls.map(f)
  }
  val optionFunctor: Functor[Option] = new Functor[Option] {
    def fmap[A, B](ma: Option[A], f: A => B) = ma.map(f)
  }

  def testFunctor() = {

    println(listFunctor.fmap(List("Hello", "Students", "!"), (s: String) => s.size))
    println(listFunctor.fmap(List("Hello", "Students", "!"), (s: String) => s.size) == List(5, 8, 1))

    import scala.collection._
    val romanNums = Map("I" -> 1, "II" -> 2, "III" -> 3) // Idea shows misleading "cannot resolve symbol" bug

    println(optionFunctor.fmap(romanNums.get("II"), (i: Int) => i * 2))
    println(optionFunctor.fmap(romanNums.get("II"), (i: Int) => i * 2) == Some(4))
    println(optionFunctor.fmap(romanNums.get("IIII"), (i: Int) => i * 2))
    println(optionFunctor.fmap(romanNums.get("IIII"), (i: Int) => i * 2) == None)
  }

  def testAll() = {
    testSeq()
    testEither()
    testFunction()
    testFunctor()
  }


}
