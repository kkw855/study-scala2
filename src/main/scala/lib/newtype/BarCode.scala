package lib.newtype

import cats._
import cats.implicits._
import io.estatico.newtype.Coercible
import io.estatico.newtype.ops._

case class Product(code: String, description: String)

object ProductRepository {
  def findByCode(barCode: String): Option[Product] =
    Some(Product(barCode, "Some description"))
  def findByDescription(description: String): Option[Product] =
    Some(Product("some-code", description))
}

case class BarCode(code: String)
case class Description(txt: String)

object AnotherProductRepository {
  def findByCode(barCode: BarCode): Option[Product] =
    Some(Product(barCode.code, "Some description"))

  def findByDescription(description: Description): Option[Product] =
    Some(Product("some-code", description.txt))
}

sealed abstract class BarCodeWithSmartConstructor(code: String)
object BarCodeWithSmartConstructor {
  def mkBarCode(code: String): Either[String, BarCodeWithSmartConstructor] =
    Either.cond(
      code.matches("\\d-\\d{6}-\\d{6}"),
      new BarCodeWithSmartConstructor(code) {},
      s"The given code $code has not the right format"
    )
}

case class BarCodeValueClass(code: String) extends AnyVal {
  def countryCode: Char = code.charAt(0)
}

object MyApp extends App {
  import io.estatico.newtype.macros.newtype

  @newtype case class BarCode(code: String)
  @newtype case class Description(desc: String)
  case class Product(code: BarCode, description: Description)

  val iPhoneBarCode: BarCode = BarCode("1-2367-123")
  val iPhoneDescription: Description = Description("Apple iPhone 12 Pro")
  val iPhone12Pro: Product = Product(iPhoneBarCode, iPhoneDescription)

  @newtype class BarCodeWithCompanion(code: String)

  val barCodeToString: Coercible[BarCode, String] = Coercible[BarCode, String]
  val stringToBarCode: Coercible[String, BarCode] = Coercible[String, BarCode]

  val code: String = barCodeToString(iPhoneBarCode)
  val iPhone12BarCode: BarCode = stringToBarCode("1-234567-890123")

  object BarCodeWithCompanion {
    def mkBarCode(code: String): Either[String, BarCodeWithCompanion] =
      Either.cond(
        code.matches("\\d-\\d{6}-\\d{6}"),
        code.coerce,
        s"The given code $code has not the right format"
      )
  }

  println(BarCodeWithCompanion.mkBarCode("1-234567-890123"))

  implicit val eq: Eq[BarCodeWithCompanion] = BarCodeWithCompanion.deriving
  implicit val show: Show[BarCodeWithCompanion] = BarCodeWithCompanion.deriving

  val res1 = (
    BarCodeWithCompanion.mkBarCode("1-234567-890123"),
    BarCodeWithCompanion.mkBarCode("2-234567-890123")
  ).mapN { case (a, b) =>
    a =!= b
  }

  val res2 = (
    BarCodeWithCompanion.mkBarCode("1-234567-890123"),
    BarCodeWithCompanion.mkBarCode("1-234567-890123")
  ).mapN { case (a, b) =>
    a === b
  }

  println(res1)
  println(res2)

  val res3 = BarCodeWithCompanion.mkBarCode("1-234567-890123").map(_.show)
  println(res3)
}
