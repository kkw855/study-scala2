package book.scala_with_cats

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import cats.syntax.either._

class EitherTest extends AnyFunSuite with Matchers {
  test("스마트 생성자") {
    3.asRight[String] shouldBe Either.right(3)

    "Negative. Stopping!".asLeft[Int] shouldBe Either.left(
      "Negative. Stopping!"
    )
  }

  test("예외 처리") {
    noException should be thrownBy Either
      .catchOnly[NumberFormatException](
        "foo".toInt
      )

    an[NumberFormatException] should be thrownBy Either
      .catchOnly[IndexOutOfBoundsException](
        "foo".toInt
      )
  }
}
