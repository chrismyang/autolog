package mypackage

import org.specs2.mutable.Specification
import TestUtils._

class LoggingTest extends Specification {
  "plugin" should {
    "work" in {

      val actualOutput = withPrintlnCaptured {
        new Test().foo(5, 6)("bar")
      }

      actualOutput must_== "Entering method foo(x: Int = 5, y: Int = 6)(z: String = bar)\nExiting method foo with return value = 10\n"
    }
  }
}
