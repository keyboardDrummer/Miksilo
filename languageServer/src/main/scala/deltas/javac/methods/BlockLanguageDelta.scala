package deltas.javac.methods

import core.deltas._
import core.deltas.grammars.{BodyGrammar, LanguageGrammars}
import core.deltas.path.{ChildPath, PathRoot}
import core.language.node.{Node, NodeField, NodeShape}
import core.language.{Compilation, Language}
import deltas.bytecode.types.{ArrayTypeDelta, UnqualifiedObjectTypeDelta, VoidTypeDelta}
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.statements.BlockDelta
import deltas.statement.StatementDelta

object BlockLanguageDelta extends DeltaWithGrammar with DeltaWithPhase
{
  object Shape extends NodeShape
  object Statements extends NodeField

  override def inject(language: Language): Unit = {
    super.inject(language)
    language.collectConstraints = (compilation, builder) => {
      val statements = PathRoot(compilation.program)(Statements).asInstanceOf[Seq[ChildPath]]
      BlockDelta.collectConstraints(compilation, builder, statements, builder.newScope(debugName = "programScope"))
    }
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val statements = find(StatementDelta.Grammar).manyVertical.as(Statements).asNode(Shape)
    find(BodyGrammar).inner = statements
  }

  override def transformProgram(program: Node, state: Compilation): Unit = {
    val statements = program(Statements).asInstanceOf[Seq[Node]]
    val mainArgument: Node = MethodDelta.parameter("args", ArrayTypeDelta.arrayType(UnqualifiedObjectTypeDelta.neww("String")))
    val method = MethodDelta.method("main",VoidTypeDelta.voidType,Seq(mainArgument), statements, static = true, AccessibilityFieldsDelta.PublicVisibility)
    val javaClass = JavaClassSkeleton.neww(Seq.empty,"Block",Seq(method))
    program.replaceWith(javaClass)
  }

  //TODO bring back. override def dependencies: Set[Contract] = Set(ImplicitObjectSuperClass, MethodDelta)

  override def description: String = "Creates a language where the program is simply a Java block."
}
