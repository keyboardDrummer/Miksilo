package deltas.javac.methods

import core.deltas._
import core.deltas.grammars.{BodyGrammar, LanguageGrammars}
import core.deltas.node.{Node, NodeField, NodeShape}
import core.deltas.path.{NodePath, NodePathRoot}
import core.language.Language
import deltas.bytecode.types.{ArrayTypeDelta, UnqualifiedObjectTypeDelta, VoidTypeDelta}
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.statements.{BlockDelta, StatementSkeleton}

object BlockLanguageDelta extends DeltaWithGrammar with DeltaWithPhase
{
  object ProgramKey extends NodeShape
  object ProgramStatements extends NodeField

  override def inject(language: Language): Unit = {
    super.inject(language)
    language.collectConstraints = (compilation, builder) => {
      val statements = NodePathRoot(compilation.program)(ProgramStatements).asInstanceOf[Seq[NodePath]]
      BlockDelta.collectConstraints(compilation, builder, statements, builder.newScope(debugName = "programScope"))
    }
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val statements = find(StatementSkeleton.StatementGrammar).manyVertical.as(ProgramStatements).asNode(ProgramKey)
    find(BodyGrammar).inner = statements
  }

  override def transformProgram(program: Node, state: Compilation): Unit = {
    val statements = program(ProgramStatements).asInstanceOf[Seq[Node]]
    val mainArgument: Node = MethodDelta.parameter("args", ArrayTypeDelta.arrayType(UnqualifiedObjectTypeDelta.neww("String")))
    val method = MethodDelta.method("main",VoidTypeDelta.voidType,Seq(mainArgument), statements, static = true, AccessibilityFieldsDelta.PublicVisibility)
    val javaClass = JavaClassSkeleton.neww(Seq.empty,"Block",Seq(method))
    program.replaceWith(javaClass)
  }

  //TODO bring back. override def dependencies: Set[Contract] = Set(ImplicitObjectSuperClass, MethodDelta)

  override def description: String = "Creates a language where the program is simply a Java block."
}
