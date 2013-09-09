package mypackage

@AutoLog
class Test extends ConsoleAutoLogging {
  def foo(x: Int, y: Int)(z: String) = {
    x + 5
  }
}

class NoLog {
  def foo(x: Int, y: Int)(z: String) = x + 5
}
