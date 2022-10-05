package book.pfps

import cats.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.{Contains, NonEmpty}
import eu.timepit.refined.refineV
import eu.timepit.refined.types.string.NonEmptyString
import io.estatico.newtype.macros.newtype

object Ch1 extends App {
  type UsernameType = NonEmptyString
  type EmailType = String Refined Contains['@']
  @newtype case class Username(value: UsernameType)
  @newtype case class Email(value: EmailType)

  println(false.guard[Option].as(111))
  println(true.guard[Option].as(111))

  println(
    (Option(1), Option(2)).mapN { case (a, b) =>
      a + b
    }
  )
  println(
    (Option(1), Option.empty[Int]).mapN { case (a, b) =>
      a + b
    }
  )

  val email = Email("kkw855@gmail.com")
  println(Username("gews"))
  println(Email("kkw855@gmail.com"))

  val str: String = "some runtime value"
  val res: Either[String, NonEmptyString] =
    refineV[NonEmpty](str)
}
