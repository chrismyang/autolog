package mypackage

import org.specs2.mutable.Specification

class TestUtilsTest extends Specification {
  "println capturing" should {
    "work" in {
      val actual = TestUtils.withPrintlnCaptured {
        println("Hello, World!")
      }

      actual must_== "Hello, World!\n"
    }
  }
}
