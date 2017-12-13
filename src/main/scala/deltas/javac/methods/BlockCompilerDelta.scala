package deltas.javac.methods

import core.deltas._
import core.deltas.grammars.{BodyGrammar, LanguageGrammars}
import core.deltas.node.{Node, NodeShape, NodeField}
import deltas.bytecode.types.{ArrayTypeC, ObjectTypeDelta, VoidTypeC}
import deltas.javac.ImplicitObjectSuperClass
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.statements.StatementSkeleton

object BlockCompilerDelta extends DeltaWithGrammar with DeltaWithPhase
{
  object ProgramKey extends NodeShape
  object ProgramStatements extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val statements = find(StatementSkeleton.StatementGrammar).manyVertical.as(ProgramStatements).asNode(ProgramKey)
    find(BodyGrammar).inner = statements
  }

  override def transformProgram(program: Node, state: Compilation): Unit = {
    val statements = program(ProgramStatements).asInstanceOf[Seq[Node]]
    val mainArgument: Node = MethodDelta.parameter("args", ArrayTypeC.arrayType(ObjectTypeDelta.objectType("String")))
    val method = MethodDelta.method("main",VoidTypeC.voidType,Seq(mainArgument), statements, static = true, AccessibilityFieldsDelta.PublicVisibility)
    val javaClass = JavaClassSkeleton.neww(Seq.empty,"Block",Seq(method))
    program.replaceWith(javaClass)
  }

  override def dependencies: Set[Contract] = Set(ImplicitObjectSuperClass, MethodDelta)

  override def description: String = "Creates a language where the program is simply a Java block."
}
