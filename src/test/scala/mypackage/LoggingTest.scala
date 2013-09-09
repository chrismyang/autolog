package mypackage

import org.specs2.mutable.Specification
import TestUtils._

class LoggingTest extends Specification {
  "plugin" should {
    "work" in {

      val (actualOutput, result) = withPrintlnCaptured {
        new Test().foo(5, 6)("bar")
      }

      result must_== 10
      actualOutput must_== "Entering method foo(x: Int = 5, y: Int = 6)(z: String = bar)\nExiting method foo with return value = 10\n"
    }

    "not add logging to class not annotated" in {

      val (actualOutput, result) = withPrintlnCaptured {
        new NoLog().foo(5, 6)("bar")
      }

      result must_== 10
      actualOutput must_== ""
    }
  }
}
