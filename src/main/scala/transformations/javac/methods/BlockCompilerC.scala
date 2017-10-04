package transformations.javac.methods

import core.particles._
import core.particles.grammars.{GrammarCatalogue, ProgramGrammar}
import core.particles.node.{Node, NodeClass, NodeField}
import transformations.bytecode.types.{ArrayTypeC, ObjectTypeDelta, VoidTypeC}
import transformations.javac.ImplicitObjectSuperClass
import transformations.javac.classes.skeleton.JavaClassSkeleton
import transformations.javac.statements.StatementSkeleton

object BlockCompilerC extends DeltaWithGrammar with DeltaWithPhase
{
  object ProgramKey extends NodeClass
  object ProgramStatements extends NodeField

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val statements = grammars.find(StatementSkeleton.StatementGrammar).manyVertical.asNode(ProgramKey, ProgramStatements)
    grammars.find(ProgramGrammar).inner = statements
  }

  override def transform(program: Node, state: Compilation): Unit = {
    val statements = program(ProgramStatements).asInstanceOf[Seq[Node]]
    val mainArgument: Node = MethodC.parameter("args", ArrayTypeC.arrayType(ObjectTypeDelta.objectType("String")))
    val method = MethodC.method("main",VoidTypeC.voidType,Seq(mainArgument), statements, static = true,MethodC.PublicVisibility)
    val clazz = JavaClassSkeleton.clazz(Seq.empty,"Block",Seq(method))
    program.replaceWith(clazz)
  }

  override def dependencies: Set[Contract] = Set(ImplicitObjectSuperClass, MethodC)

  override def description: String = "Creates a language where the program is simply a Java block."
}
