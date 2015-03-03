package transformations.javac.classes

import core.transformation.Contract
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation

object FieldDeclaration extends GrammarTransformation {

  override def dependencies: Set[Contract] = super.dependencies ++ Set(ClassC)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val memberGrammar = grammars.find(ClassC.ClassMemberGrammar)
    val fieldGrammar =
  }
}
