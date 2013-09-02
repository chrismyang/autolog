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
        case Literal(Constant(str: String)) => Literal(Constant("ICanHazYourStrngLiterls"))

        // don't forget this case, so that tree is actually traversed
        case _ => super.transform(tree)
      }
    }
  }
}
