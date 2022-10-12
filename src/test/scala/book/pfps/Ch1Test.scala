package book.pfps

import cats._
import cats.implicits._

import book.pfps.Ch1.{Email, Username}
import eu.timepit.refined.types.string.NonEmptyString
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

//noinspection ScalaUnusedSymbol
class Ch1Test extends AnyFunSuite with Matchers {
  import eu.timepit.refined.auto._

  test("new 생성자 사용 불가") {
    """new Username("abc")""" shouldNot compile
    """new Email("abc@gmail.com")""" shouldNot compile
  }

  test("copy 함수 사용 불가") {
    """Username("abc").copy("zzz")""" shouldNot compile
    """Email("abc@gmail.com").copy("zzz@naver.com")""" shouldNot compile
  }

  test("컴파일 타임 - 입력 데이터 유효성 검사") {
    """Username("")""" shouldNot compile
    """Email("abc")""" shouldNot compile

    Username("a")
    Email("@")
  }

  test("런타임 - 입력 데이터 유효성 검사") {
    val str: String = ""

    NonEmptyString.from(str) shouldBe "Predicate isEmpty() did not fail."
      .asLeft[String]
  }
}
