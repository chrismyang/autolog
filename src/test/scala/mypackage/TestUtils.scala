package mypackage

import java.io.{PrintStream, ByteArrayOutputStream}

object TestUtils {
  def withPrintlnCaptured[A](f: => A): (String, A) = {
    val original = Console.out

    val capturedStdout = new ByteArrayOutputStream()
    val result = try {
      Console.setOut(new PrintStream(capturedStdout))
      f
    } finally {
      Console.setOut(original)
    }

    (new String(capturedStdout.toByteArray), result)
  }
}
