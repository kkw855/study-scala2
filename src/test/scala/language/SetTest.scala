package language

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SetTest extends AnyFunSuite with Matchers {
  private val set1 = Set(1, 2, 3)
  private val set2 = Set(3, 4)

  test("합집합") {
    set1 union Set.empty[Int] shouldBe set1
    set1 union set2 shouldBe Set(1, 2, 3, 4)
  }

  test("교집합") {
    set1 intersect Set.empty[Int] shouldBe Set.empty[Int]
    set1 intersect set2 shouldBe Set(3)
  }

  test("차집합") {
    set1 diff set2 shouldBe Set(1, 2)
    set2 diff set1 shouldBe Set(4)
  }
}
