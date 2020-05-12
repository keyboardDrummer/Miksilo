package miksilo.modularLanguages.deltas.javac.methods

import miksilo.modularLanguages.core.deltas._
import miksilo.modularLanguages.core.deltas.grammars.{BodyGrammar, LanguageGrammars}
import miksilo.modularLanguages.core.deltas.path.PathRoot
import miksilo.modularLanguages.core.node.{Node, NodeField, NodeShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.core.SolveConstraintsDelta
import miksilo.modularLanguages.deltas.bytecode.types.{ArrayTypeDelta, UnqualifiedObjectTypeDelta, VoidTypeDelta}
import miksilo.modularLanguages.deltas.javac.classes.skeleton.JavaClassDelta
import miksilo.modularLanguages.deltas.method.MethodDelta
import miksilo.modularLanguages.deltas.statement.{BlockDelta, StatementDelta}

object BlockLanguageDelta extends DeltaWithGrammar with DeltaWithPhase
{
  object Shape extends NodeShape
  object Statements extends NodeField

  override def inject(language: Language): Unit = {
    super.inject(language)
    SolveConstraintsDelta.constraintCollector.add(language, (compilation, builder) => {
      val block = compilation.program.asInstanceOf[PathRoot]
      BlockDelta.collectConstraints(compilation, builder, block, builder.newScope(debugName = "programScope"))
    })
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
    compilation.program = javaClass
  }

  //TODO bring back. override def dependencies: Set[Contract] = Set(ImplicitObjectSuperClass, MethodDelta)

  override def description: String = "Creates a language where the program is simply a Java block."

  override def dependencies: Set[Contract] = Set(BlockDelta)
}
