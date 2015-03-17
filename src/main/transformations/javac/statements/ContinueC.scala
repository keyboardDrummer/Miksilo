package transformations.javac.statements

import core.particles.{MetaObjectWithOrigin, CompilationState, MetaObject}
import core.particles.grammars.GrammarCatalogue
import transformations.bytecode.additions.LabelledTargets

object ContinueC extends StatementInstance {
  override val key: AnyRef = ContinueKey

  object ContinueKey
  def continue = new MetaObject(ContinueKey)

  override def toByteCode(statement: MetaObjectWithOrigin, state: CompilationState): Seq[MetaObject] = {
    val startLabel = WhileC.getState(state).whileStartLabels(getWhileParent(statement))
    Seq(LabelledTargets.goTo(startLabel))
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val statementGrammar = grammars.find(StatementSkeleton.StatementGrammar)
    statementGrammar.addOption("continue;" ~> produce(continue))
  }

  override def description: String = "Jumps the program to the start of the loop."

  override def getNextStatements(obj: MetaObjectWithOrigin, labels: Map[Any, MetaObjectWithOrigin]): Set[MetaObjectWithOrigin] = {
    val _whileParent: MetaObjectWithOrigin = getWhileParent(obj)
    Set(labels(WhileC.startKey(_whileParent)))
  }

  def getWhileParent(obj: MetaObjectWithOrigin): MetaObjectWithOrigin = {
    val ancestors = obj.origin.ancestors
    val _whileParent = ancestors.filter(ancestor => ancestor.clazz == WhileC.WhileKey).head
    _whileParent
  }
}
