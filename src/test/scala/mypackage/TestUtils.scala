package mypackage

import java.io.{PrintStream, ByteArrayOutputStream}

object TestUtils {
  def withPrintlnCaptured(f: => Unit): String = {
    val original = Console.out

    val capturedStdout = new ByteArrayOutputStream()
    try {
      Console.setOut(new PrintStream(capturedStdout))
      f
    } finally {
      Console.setOut(original)
    }

    new String(capturedStdout.toByteArray)
  }
}
