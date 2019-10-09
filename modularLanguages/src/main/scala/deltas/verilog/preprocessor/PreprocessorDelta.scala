package deltas.verilog.preprocessor

import core.bigrammar.grammars.Keyword
import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.{NodePath, PathRoot}
import core.language.node.{GrammarKey, Node}
import core.language.{Compilation, Language}
import deltas.FileWithMembersDelta

object PreprocessorDelta extends DeltaWithPhase with DeltaWithGrammar {

  override def description: String = "Enable preprocessor directives that start with `" +
    " and can appear everywhere where trivia can occur"

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val preprocessor = new Preprocessor(compilation)
    PathRoot(program).visit(beforeChildren = path => {
      for (transformation <- transformations.get(compilation, path.shape)) {
        transformation.apply(preprocessor, path)
      }
      true
    })
  }

  override def dependencies: Set[Contract] = Set.empty

  val transformations = new ShapeProperty[Directive]
  object Grammar extends GrammarKey
  object BodyGrammar extends GrammarKey
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {

    val directiveGrammar = Keyword("`") ~> grammars.create(BodyGrammar)
    grammars.find(FileWithMembersDelta.Members).addAlternative(grammars.create(Grammar, directiveGrammar))
  }
}

trait DirectiveDelta extends Delta with Directive with HasShape with DeltaWithGrammar {
  override def inject(language: Language): Unit = {
    super.inject(language)
    PreprocessorDelta.transformations.add(language, this)
  }
}

case class Macro(name: String, method: Seq[Node] => Node)
class Preprocessor(val compilation: Compilation) {
  var macros = Map.empty[String, Macro]

  def addMacro(_macro: Macro): Unit = macros += (_macro.name -> _macro)
}

trait Directive {
  def apply(preprocessor: Preprocessor, path: NodePath): Unit
}


