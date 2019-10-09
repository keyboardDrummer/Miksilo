package deltas.javac.methods

import core.deltas._
import core.deltas.grammars.{BodyGrammar, LanguageGrammars}
import core.deltas.path.PathRoot
import core.language.node.{Node, NodeField, NodeShape}
import core.language.{Compilation, Language}
import deltas.bytecode.types.{ArrayTypeDelta, UnqualifiedObjectTypeDelta, VoidTypeDelta}
import deltas.javac.classes.skeleton.JavaClassDelta
import deltas.statement.{BlockDelta, StatementDelta}

object BlockLanguageDelta extends DeltaWithGrammar with DeltaWithPhase
{
  object Shape extends NodeShape
  object Statements extends NodeField

  override def inject(language: Language): Unit = {
    super.inject(language)
    language.collectConstraints = (compilation, builder) => {
      val block = PathRoot(compilation.program)
      BlockDelta.collectConstraints(compilation, builder, block, builder.newScope(debugName = "programScope"))
    }
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val statements = find(StatementDelta.Grammar).manyVertical.as(BlockDelta.Statements).asNode(BlockDelta.Shape)
    find(BodyGrammar).inner = statements
  }

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val block = program
    val mainArgument: Node = MethodParameters.neww("args", ArrayTypeDelta.neww(UnqualifiedObjectTypeDelta.neww("String")))
    val method = MethodDelta.neww("main",VoidTypeDelta.voidType,Seq(mainArgument), block, static = true, AccessibilityFieldsDelta.PublicVisibility)
    val javaClass = JavaClassDelta.neww(Seq.empty,"Block",Seq(method))
    program.replaceData(javaClass)
  }

  //TODO bring back. override def dependencies: Set[Contract] = Set(ImplicitObjectSuperClass, MethodDelta)

  override def description: String = "Creates a language where the program is simply a Java block."

  override def dependencies: Set[Contract] = Set(BlockDelta)
}
