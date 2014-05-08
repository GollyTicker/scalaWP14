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
    println(None == sequence(List(Some(1), Some(2), None)))
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

  def sortEither(ls: List[Either[Person, Company]]): List[Either[Person, Company]] = ls match {
    case Nil => Nil
    case pivot :: xs => {
      val smaller = xs.filter(e => cmpEither(e, pivot) <= 0)
      val bigger = xs.filter(e => cmpEither(e, pivot) > 0)
      smaller ++ List(pivot) ++ bigger
    }
  }

  def cmpEither(fst: Either[Person, Company], snd: Either[Person, Company]): Int = () match {
    //case _ if fst.isLeft && snd.isLeft => fst compare snd // compiliert nicht...... diverging implitic expansion for type ....
    //case _ if fst.isRight && snd.isRight => fst compare snd
    case _ => -1
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

    println(sortEither(customers) == sortResult)
    println(sortEither(customers))

    println(selectPersons(customers) == persons)
    println(selectPersons(customers))

    println(selectCompanies(customers) == companies)
    println(selectCompanies(customers))


  }

  trait ~>[A, B] extends (A => B) {
    def applyOrDefault(x:A, default:B):B
  }

  def use1[A, B](f: A => B)(x: A):B = f(x)

  def use2[A, B](f: A ~> B)(x: A)(default: B):B =
    f.applyOrDefault(x, default)

  val f: Double ~> Double = new (Double => Double) with (Double ~> Double) {
    def applyOrDefault(x:Double, default:Double):Double = if (x >= 0) this(x) else default
    def apply(x:Double):Double = math.sqrt(x)
  }

  def testFunction() = {
    println( use1(f) (2) )
    println( use1(f) (2) == math.sqrt(2) )

    println( use1(f) (-2.0) )
    println( use1(f) (-2.0) != Double.NaN ) // NaN != NaN => true

    println( use2(f) (0.0) (-2.0) )
    println( use2(f) (0.0) (-2.0) == 0.0 )
  }
}
