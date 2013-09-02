package mypackage

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.Global
import scala.tools.nsc.transform.Transform

// a sample component which is a transformer
// which replaces all literal string constants
// in the compiled sources
class ExampleComponent(val global: Global) extends PluginComponent with Transform {
  import global._

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
        new LoggingDecorator(tree).withDecoration

      // don't forget this case, so that tree is actually traversed
      case _ => super.transform(tree)
    }
  }

  class LoggingDecorator(method: DefDef) {
    def withDecoration: Tree = {
      val modifiedTree = addExitStatement(addEnteringStatement(method.rhs))
      method.copy(rhs = modifiedTree)
    }

    private def renderMethodParameters: Tree = {
      val y = for {
        parameterList <- method.vparamss
      } yield {
        renderParameterList(parameterList)
      }

      val x = List.fill(y.size)("(%s)").mkString

      Apply(
        Select(
          Literal(Constant(x)),
          newTermName("format")
        ),
        y
      )
    }

    private def renderParameterList(parameterList: List[ValDef]): Tree = {
      val y = for {
        parameter <- parameterList
      } yield {
        perArgument(parameter)
      }

      val x = List.fill(y.size)("%s").mkString(", ")

      Apply(
        Select(
          Literal(Constant(x)),
          newTermName("format")
        ),
        y
      )
    }

    private def perArgument(x: ValDef): Tree = {
      Apply(
        Select(
          Literal(Constant("%s: %s = %%s".format(x.name, x.tpt))),
          newTermName("format")),
        List(
          Ident(x.name)
        )
      )
    }

    private def addEnteringStatement(rhs: Tree): Tree = {

      val stringToPrint = "Entering method %s".format(method.name)

      val treeToPrintln = Apply(
        Select(Literal(Constant(stringToPrint)), newTermName("$plus")),
        List(renderMethodParameters)
      )

      val enteringStatement = Apply(
        Ident(newTermName("println")),
        List(
          treeToPrintln
        )
      )

      Block(
        List(enteringStatement),
        rhs
      )
    }

    private def addExitStatement(rhs: Tree): Tree = {
      val resultValueTermName = newTermName("$result")
      val saveOriginalExitValue = ValDef(NoMods, resultValueTermName, TypeTree(), rhs)

      val printExitStatement = makeExitStatement(resultValueTermName)

      val returnOriginalExitValue = Ident(resultValueTermName)

      Block(List(saveOriginalExitValue, printExitStatement), returnOriginalExitValue)
    }

    private def makeExitStatement(resultValueTermName: TermName): Tree = {
      Apply(
        Ident(newTermName("println")),
        List(
          Apply(
            Select(Literal("Exiting method %s with return value = ".format(method.name)), newTermName("$plus")),
            List(Ident(resultValueTermName))
          )
        )
      )
    }
  }
}
