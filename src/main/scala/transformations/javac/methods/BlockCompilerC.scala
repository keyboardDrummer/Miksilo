package transformations.javac.methods

import core.particles.grammars.{GrammarCatalogue, ProgramGrammar}
import core.particles._
import core.particles.node.{Key, Node}
import transformations.javac.ImplicitObjectSuperClass
import transformations.javac.classes.skeleton.JavaClassSkeleton
import transformations.javac.statements.StatementSkeleton
import transformations.bytecode.types.{ArrayTypeC, ObjectTypeC, VoidTypeC}

object BlockCompilerC extends DeltaWithGrammar with DeltaWithPhase
{
  object ProgramKey extends Key
  object ProgramStatements extends Key

  override def transformGrammars(grammars: GrammarCatalogue, state: CompilationState): Unit = {
    val statements = grammars.find(StatementSkeleton.StatementGrammar).manyVertical.asNode(ProgramKey, ProgramStatements)
    grammars.find(ProgramGrammar).inner = statements
  }

  override def transform(program: Node, state: CompilationState): Unit = {
    val statements = program(ProgramStatements).asInstanceOf[Seq[Node]]
    val mainArgument: Node = MethodC.parameter("args", ArrayTypeC.arrayType(ObjectTypeC.objectType("String")))
    val method = MethodC.method("main",VoidTypeC.voidType,Seq(mainArgument), statements, static = true,MethodC.PublicVisibility)
    val clazz = JavaClassSkeleton.clazz(Seq.empty,"Block",Seq(method))
    program.replaceWith(clazz)
  }

  override def dependencies: Set[Contract] = Set(ImplicitObjectSuperClass, MethodC)

  override def description: String = "Creates a language where the program is simply a Java block."
}
