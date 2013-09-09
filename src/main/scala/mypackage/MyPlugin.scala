package mypackage

import scala.tools.nsc
import nsc.Global
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

class MyPlugin(val global: Global) extends Plugin {
  val name = "MyPlugin"
  val description = "My first Scala compiler plugin".format("foo")
  val components = List[PluginComponent](new ExampleComponent(global))
}

/**

  def foo(x: Int) = {
    println("Entering...")
    val y = x + 5
    println("Exiting...")
    y
  }
  **/