package mypackage

trait AutoLogging {
  def onMethodExit(methodName: String, finalValue: Any)
}

trait ConsoleAutoLogging extends AutoLogging {
  def onMethodEnter(methodName: String) {

  }

  def onMethodExit(methodName: String, finalValue: Any) {
    println("Exiting method %s with return value = %s".format(methodName, finalValue))
  }
}