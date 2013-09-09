package mypackage

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.Global
import scala.tools.nsc.transform.{TypingTransformers, Transform}
import scala.tools.nsc.ast.TreeDSL

// a sample component which is a transformer
// which replaces all literal string constants
// in the compiled sources
class ExampleComponent(val global: Global) extends PluginComponent with Transform with TypingTransformers with TreeDSL {
  import global._

  // TODO: change that according to your requirements
  override val runsAfter = List("typer")

  /** The phase name of the compiler plugin
    *  @todo Adapt to specific plugin.
    */
  val phaseName = "MyPlugin"

  def newTransformer(unit: CompilationUnit) = new $name$Transformer(unit)

  class $name$Transformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    import CODE._

    // TODO: fill in your logic here
    override def transform(tree: Tree): Tree = tree match {
//      case tree @ ClassDef(mods, name, tparams, impl) if shouldLoggify(tree) =>
//        println(tree)
//        tree

      case tree @ DefDef(mods, name, tparams, vparamss, tpt, rhs) if shouldLoggify(tree) =>

        val x = treeCopy.DefDef(
          tree,
          mods,
          name,
          tparams,
          vparamss,
          tpt,
          localTyper.typed(
            atPos(tree.pos)(
              {


                val resultValueTermName = tree.symbol.newValue(tree.pos, newTermName("$result")).setInfo(rhs.tpe)

                val executeBodyAndSaveResult = VAL(resultValueTermName) === rhs

                //        val printExitStatement = makeExitStatement(resultValueTermName)


//                val println = Select(qualifier, tree.symbol.newMethod(tree.pos, newTermName("println")))

                val onExit = Apply(
                  Select(This(currentOwner), newTermName("onMethodExit")),
                  List(
                    Literal(name.toString()),
                    Ident(resultValueTermName)
                  )
                )

                Block(
                  List(
                    executeBodyAndSaveResult,
                    onExit
                  ),
                  // Ident(resultValueTermName)
                  Ident(resultValueTermName)
                )
              }
            )
          )
        )

        x







//        val decoration = new LoggingDecorator(tree).withDecoration
//        val typedTree = localTyper.typed(decoration)


      // don't forget this case, so that tree is actually traversed
      case _ => super.transform(tree)
    }

//    private def shouldLoggify(tree: ClassDef): Boolean = {
//      val ajkdf = tree.mods.annotations.map(typer.typedAnnotation(_))
//
//      tree.mods.annotations.exists { x =>
//        x.symbol.annotations.exists { y =>
//          y.atp.safeToString == classOf[AutoLog].toString
//        }
//      }
//    }

    private def shouldLoggify(tree: DefDef): Boolean = {
      containingClassIsAutolog && !isConstructor(tree)
    }

    private def containingClassIsAutolog = {
      val y = currentClass
      val z = y.annotations

      z.exists { x =>
        println(x.atp.safeToString)
        x.atp.safeToString == classOf[AutoLog].getCanonicalName
      }
    }

    private def isConstructor(tree: DefDef) = tree.name.toString == "<init>"

    class LoggingDecorator(method: DefDef) {
      def withDecoration: Tree = {
  //      val modifiedBody = addExitStatement(addEnteringStatement(method.rhs))
        val modifiedBody = addExitStatement(method.rhs)
        treeCopy.DefDef(method, method.mods, method.name, method.tparams, method.vparamss, method.tpt, modifiedBody)
      }

//      private def addEnteringStatement(originalBody: Tree): Tree = {
//        Block(
//          List(Println(StringPlus("Entering method %s".format(method.name), renderMethodParameters))),
//          originalBody
//        )
//      }

      private def addExitStatement(body: Tree): Tree = {

        val resultValueTermName = newTermName("$result")

        val executeBodyAndSaveResult = VAL(resultValueTermName, body.tpe) === body

//        val printExitStatement = makeExitStatement(resultValueTermName)

        val returnOriginalExitValue = Ident(resultValueTermName).setType(body.tpe)

        BLOCK(executeBodyAndSaveResult, returnOriginalExitValue)
      }

//      private def renderMethodParameters: Tree = {
//        val y = for {
//          parameterList <- method.vparamss
//        } yield {
//          renderParameterList(parameterList)
//        }
//
//        val x = List.fill(y.size)("(%s)").mkString
//
//        Format(x, y)
//      }

//      private def renderParameterList(parameterList: List[ValDef]): Tree = {
//        val y = for {
//          parameter <- parameterList
//        } yield {
//          perArgument(parameter)
//        }
//
//        val x = List.fill(y.size)("%s").mkString(", ")
//
//        Format(x, y)
//      }

//      private def perArgument(x: ValDef): Tree = StringPlus("%s: %s = ".format(x.name, x.tpt), Ident(x.name))

//      private def makeExitStatement(resultValueTermName: TermName): Tree = {
//        Println(StringPlus("Exiting method %s with return value = ".format(method.name), Ident(resultValueTermName)))
//      }

//      private def Println(arg: Tree) = {
//        Apply(
//          Ident(newTermName("println")),
//          List(
//            arg
//          )
//        )
//      }

//      private def StringPlus(base: String, argument: Tree) = {
//        localTyper.typed(Apply(
//          Select(Literal(base), newTermName("$plus")),
//          List(argument)
//        ))
//      }
//
//      private def Format(formatString: String, arguments: List[Tree]) = {
//          Apply(
//            Select(
//              Select(Select(Ident(newTermName("java")), newTermName("lang")), newTermName("String")),
//              newTermName("format")
//            ),
//            Literal(Constant(formatString)) +: arguments
//          )
//      }
    }
  }

}
