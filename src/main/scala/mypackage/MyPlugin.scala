package mypackage

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import nsc.transform.{ Transform, TypingTransformers }
import nsc.symtab.Flags

class MyPlugin(val global: Global) extends Plugin {
  import global._

  val name = "MyPlugin"
  val description = "My first Scala compiler plugin"
  val components = List[PluginComponent](ExampleComponent)

  // a sample component which is a transformer
  // which replaces all literal string constants
  // in the compiled sources
  private object ExampleComponent extends PluginComponent with Transform {

    import global._

    val global = MyPlugin.this.global

    // TODO: change that according to your requirements
    override val runsAfter = List("parser")

    /** The phase name of the compiler plugin
     *  @todo Adapt to specific plugin.
     */
    val phaseName = "MyPlugin"

    def newTransformer(unit: CompilationUnit) = new $name$Transformer(unit)

    class $name$Transformer(unit: CompilationUnit) extends Transformer {

      // TODO: fill in your logic here
      override def transform(tree: Tree): Tree = tree match {
        case tree @ DefDef(mods, name, tparams, vparamss, tpt, rhs) if name.toString == "foo" =>
          val modifiedTree = addExitStatement(addEnteringStatement(rhs))
          tree.copy(rhs = modifiedTree)

        // don't forget this case, so that tree is actually traversed
        case _ => super.transform(tree)
      }
    }

    private def makePrintlnStatement(stringToPrint: String) = Apply(Ident(newTermName("println")), List(Literal(Constant(stringToPrint))))

    private def addEnteringStatement(rhs: Tree): Tree = {
      Block(List(makePrintlnStatement("Entering...")), rhs)
    }

    private def addExitStatement(rhs: Tree): Tree = {
      val resultValueTermName = newTermName("$result")
      val saveOriginalExitValue = ValDef(NoMods, resultValueTermName, TypeTree(), rhs)

      val printExitStatement = makePrintlnStatement("Exiting...")

      val returnOriginalExitValue = Ident(resultValueTermName)

      Block(List(saveOriginalExitValue, printExitStatement), returnOriginalExitValue)
    }
  }
}

/**

  def foo(x: Int) = {
    println("Entering...")
    val y = x + 5
    println("Exiting...")
    y
  }
  **/