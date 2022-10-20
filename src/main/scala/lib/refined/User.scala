package lib.refined

import cats._
import cats.data._
import cats.implicits._
import eu.timepit.refined.api.{RefType, Refined, RefinedTypeOps}
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import eu.timepit.refined.string._
import eu.timepit.refined.W
import eu.timepit.refined.types.string.NonEmptyString

case class User(name: User.SimpleName, email: User.Email)

object User {
  type SimpleName = String Refined MatchesRegex[W.`"""[A-Z][a-z]+"""`.T]
  type Email =
    String Refined MatchesRegex[W.`"""[a-z0-9]+@[a-z0-9]+\\.[a-z0-9]{2,}"""`.T]
}

object MyApp extends App {
  val aPositiveInteger: Refined[Int, Positive] = 42
  val aNegative: Int Refined Negative = -100
  val nonNegative: Int Refined NonNegative = 0
  val anOdd: Int Refined Odd = 3
  val anEven: Int Refined Even = 68

  val commandPrompt: String Refined EndsWith[W.`"$"`.T] = "daniel@mbp $"
//  val commandPrompt2: String Refined EndsWith[W.`"$"`.T] = "daniel@mbp"

  User("Daniel", "daniel@rockthejvm.com")
  // User("daniel@rockthejvm.com", "Daniel")

  val poorEmail = "daniel"
  val correctEmail = "daniel@rockthejvm.com"
  val refineCheck = RefType.applyRef[User.Email](poorEmail)
  val refineCheck2 = RefType.applyRef[User.Email](correctEmail)

  println(refineCheck)
  println(refineCheck2)

  val str = ""
  val str2 = "a"
  println(NonEmptyString.from(str))
  println(NonEmptyString.from(str2))

  type GTFive = Int Refined Greater[5]
  object GTFive extends RefinedTypeOps[GTFive, Int]

  val number: Int = 33
  val res: Either[String, GTFive] = GTFive.from(number)
  println(res)
  val number2 = 5
  val res2: Either[String, GTFive] = GTFive.from(number2)
  println(res2)

  case class MyType(a: NonEmptyString, b: GTFive)

  def validate(a: String, b: Int): ValidatedNel[String, MyType] =
    (
      NonEmptyString.from(a).toValidatedNel,
      GTFive.from(b).toValidatedNel
    ).mapN(MyType.apply)

  println(validate("", 3))
}
