package book.scala_with_cats

import cats.Applicative

object Ch1 extends App {
  // Define a very simple JSON AST
  sealed trait Json
  final case class JsObject(get: Map[String, Json]) extends Json
  final case class JsString(get: String) extends Json
  final case class JsNumber(get: Double) extends Json
  final case object JsNull extends Json

  // The "serialize to JSON" behaviour is encoded in this trait
  trait JsonWriter[A] {
    def write(value: A): Json
  }

  final case class Person(name: String, email: String)

  object JsonWriterInstances {
    implicit val stringWriter: JsonWriter[String] =
      (value: String) => JsString(value)
    implicit val personWriter: JsonWriter[Person] =
      (value: Person) =>
        JsObject(
          Map(
            "name" -> JsString(value.name),
            "email" -> JsString(value.email)
          )
        )
  }

  // Interface object
  object Json {
    def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
      w.write(value)
  }

  // Interface syntax
  object JsonSyntax {
    implicit class JsonWriterOps[A](value: A) {
      def toJson(implicit w: JsonWriter[A]): Json =
        w.write(value)
    }
  }

  import JsonWriterInstances._
  import JsonSyntax._

  // Interface object
  Json.toJson(Person("Dave", "dave@example.com"))

  // Interface syntax
  Person("Dave", "dave@example.com").toJson

  //  implicitly Method
  implicitly[JsonWriter[String]]

  implicit def optionWriter[A](implicit
      writer: JsonWriter[A]
  ): JsonWriter[Option[A]] = {
    case Some(aValue) => writer.write(aValue)
    case None         => JsNull
  }

  Json.toJson(Option("A string"))

  trait Printable[A] { self =>
    def format(value: A): String

    def contramap[B](func: B => A): Printable[B] =
      (value: B) => self.format(func(value))
  }

  object Printable {
    def format[A](value: A)(implicit printer: Printable[A]): String =
      printer.format(value)

    def print[A](value: A)(implicit printer: Printable[A]): Unit =
      println(printer.format(value))
  }

  // Interface object
  object PrintableInstances {
    implicit val intPrintable: Printable[Int] =
      String.valueOf

    implicit val stringPrintable: Printable[String] =
      v => s"'$v'"

    implicit val booleanPrintable: Printable[Boolean] =
      if (_) "yes" else "no"

    implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
      p.contramap[Box[A]](_.value)
  }
  // Interface syntax
  object PrintableSyntax {
    implicit class PrintableOps[A](value: A) {
      def format(implicit p: Printable[A]): String = p.format(value)
      def print(implicit p: Printable[A]): Unit = println(format(p))
    }
  }

  import PrintableInstances._
  import PrintableSyntax._

  println(Printable.format("ABC"))
  println(Printable.format(123))
  Printable.print("ABC")
  Printable.print(123)

  // Define a cat
  final case class Cat(name: String, age: Int, color: String)
  object Cat {
    implicit val catPrintable: Printable[Cat] =
      cat => {
        val name = Printable.format(cat.name)
        val age = Printable.format(cat.age)
        val color = Printable.format(cat.color)
        s"$name is a $age year-old $color cat."
      }
  }

  // Print the cat
  val cat = Cat("Garfield", 41, "ginger and black")
  Printable.print(cat)
  cat.print

  // Meet cats
  import cats.Show

  val showDouble = Show.apply[Double]
  val showList = Show.apply[List[Int]]

  println(showDouble.show(3.14))
  println(showList.show(List(1, 2, 3)))
  println(showDouble)
  println(cats.instances.double.catsStdShowForDouble)

  // import interface syntax
  import cats.syntax.show._

  println("abc".show)

  // The fact that this code compiles is a design flaw of the Java API.
  val root: Any = new {}
  println(root.toString)

  implicit val showPerson: Show[Person] = Show.show(_.name)
  case class Department(id: Int, name: String)
  implicit val showDepartment: Show[Department] = Show.fromToString

  val john = Person("John", "john@gmail.com")
  val engineering = Department(2, "Engineering")

  println(s"$john") // Using toString method
  println(show"$john") // Using show method

  implicit val showCat: Show[Cat] = Show.show { cat =>
    show"${cat.name} is a ${cat.age} year-old ${cat.color} cat."
  }
  println(cat.show)

  // Type class Eq
  import cats.Eq
  import cats.syntax.eq._
  import cats.syntax.option._

  //noinspection ComparingUnrelatedTypes
  println(cat.equals(john))

  val eqInt = Eq[Int]
  println(eqInt.eqv(123, 123))
  println(123 === 123)

  println(Option(1) === Option.empty[Int])
  println(1.some === none[Int])

  implicit val eqCat: Eq[Cat] = Eq.instance { (a, b) =>
    a.name === b.name && a.age === b.age && a.color == b.color
  }
  val cat1 = Cat("Garfield", 38, "orange and black")
  // val cat2 = Cat("Garfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")

  println(cat1 === cat2)
  println(cat1 =!= cat2)

  // Type class Semigroup and Monoid
  import cats.Monoid
  import cats.syntax.monoid._

  def associativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean = {
    m.combine(x, m.combine(y, z)) ==
      m.combine(m.combine(x, y), z)
  }

  def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean = {
    (m.combine(x, m.empty) == x) &&
    (m.combine(m.empty, x) == x)
  }

//  implicit val monoidBooleanAnd: Monoid[Boolean] =
//    Monoid.instance(true, (a, b) => a && b)

  implicit val monoidBooleanOr: Monoid[Boolean] =
    Monoid.instance(false, (a, b) => a || b)

  println(identityLaw(true))
  println(identityLaw(false))
  println(associativeLaw(true, true, false))
  println(associativeLaw(false, true, true))

  println(Monoid[String].combine("Hi ", "there"))
  println(Monoid[String].empty)

  println(Monoid[Option[Int]].combine(Option(22), Option(20)))
  println(Monoid[Option[Int]].combine(None, Option(20)))

  def add[A: Monoid](items: List[A]): A =
    items.foldLeft(Monoid[A].empty)(_ |+| _)

  println(add(List(1, 2, 3)))
  println(add(List(Some(1), None, Some(2), None, Some(3))))

  case class Order(totalCost: Double, quantity: Double)
  object Order {
    implicit val monoidOrder: Monoid[Order] =
      Monoid.instance(
        Order(0, 0),
        (a, b) => Order(a.totalCost + b.totalCost, a.quantity + b.quantity)
      )
  }

  println(add(List(Order(3, 4), Order(1, 2))))

  import cats.Functor
  import cats.syntax.functor._

  val func1 = (a: Int) => a + 1
  val func2 = (a: Int) => a * 2
  val func3 = (a: Int) => s"${a}!"
  val func4 = func1 map func2 map func3
  func4(123)

  def doMath[F[_]: Functor](start: F[Int]): F[Int] =
    start map { n => n + 1 * 2 }

  println(doMath(Option(20)))
  println(doMath(List(1, 2, 3)))

  sealed trait Tree[+A]
  object Tree {
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
      Branch(left, right)

    def leaf[A](value: A): Tree[A] =
      Leaf(value)
  }
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  implicit val treeFunctor: Functor[Tree] =
    new Functor[Tree] {
      def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
        case Leaf(value)         => Leaf(f(value))
      }
    }

  println(Tree.leaf(100).map(_ * 2))
  println(Tree.branch(Leaf(10), Leaf(20)).map(_ * 2))

  println(Printable.format("hello"))
  println(Printable.format(true))

  final case class Box[A](value: A)

  println(Printable.format(Box("hello world")))
  println(Printable.format(Box(true)))

  import cats.Monad
  import cats.syntax.functor._
  import cats.syntax.flatMap._
  import cats.Id

  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    for {
      x <- a
      y <- b
    } yield x * x + y * y

  println(sumSquare(Option(3), Option(4)))
  println(sumSquare(List(1, 2, 3), List(4, 5)))
  println(sumSquare(3: Id[Int], 4: Id[Int]))

  import cats.syntax.either._

  val either1: Either[String, Int] = Right(10)
  val either2: Either[String, Int] = Right(32)

  for {
    a <- either1
    b <- either2
  } yield a + b

  // println(Either.catchOnly[IndexOutOfBoundsException]("foo".toInt))
  println(Either.catchOnly[NumberFormatException]("123".toInt))
  println(Either.catchNonFatal(sys.error("Badness")))
}
