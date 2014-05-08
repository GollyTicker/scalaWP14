import scala.math.Ordering

/**
 * Created by Swaneet on 08.05.2014.
 */
object A8 {

  def sequence[A](ls:List[Option[A]]):Option[List[A]] = ls match {
    case Nil      => Some(Nil)
    case mx :: mx_s  => for {
      x <- mx
      xs <- sequence[A](mx_s)
    } yield (x :: xs)
  }

  def testSeq() {
    println(None == sequence(List(Some(1),Some(2),None)))
    println(Some(List("Hallo", "Welt")) == sequence(List(Some("Hallo"),Some("Welt"))))
  }


  case class Person(name:String) extends Ordered[Person] {
    override def compare(that:Person): Int =  name compare (that.name)
  }
  case class Company(p:Person) extends Ordered[Company]  {
    override def compare(that:Company): Int =  p compare (that.p)
  }

  val customers = List(Left(Person("P1")),Left(Person("P2")), Right(Company(Person("CEO1"))), Left(Person("P3")),
    Right(Company(Person("CEO2"))),Right(Company(Person("CEO3"))), Left(Person("P3")))

  def sortEither[A, B](ls:List[Either[A,B]]):List[Either[A,B]] =
     selectPersons(ls).sorted.map( Left(_)) ++ selectCompanies(ls).sorted.map( Right(_))

  val sorted = (List(Left(Person("P1")), Left(Person("P2")), Left(Person("P3")),Left(Person("P3"))),
    List(Right(Company(Person("CEO1"))), Right(Company(Person("CEO2"))), Right(Company(Person("CEO3")))))

  def selectPersons[A, B](ls:List[Either[A,B]]):List[A] = ???

  val persons = List(Person("P1"), Person("P2"), Person("P3"), Person("P3"))

  def selectCompanies[A, B](ls:List[Either[A,B]]):List[B] = ???

  val companies = List(Company(Person("CEO1")), Company(Person("CEO2")), Company(Person("CEO3")))

  def testEither() {

    println(customers + "\n")
    println(sortEither(customers) == sorted)
    println(sortEither(customers) + "\n")

    println(selectPersons(customers) == persons)
    println(selectPersons(customers) + "\n")

    println(selectCompanies(customers) == companies)
    println(selectCompanies(customers) + "\n")


  }

}
