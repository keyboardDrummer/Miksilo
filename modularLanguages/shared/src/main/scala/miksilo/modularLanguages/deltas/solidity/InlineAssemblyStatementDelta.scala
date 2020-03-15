package miksilo.modularLanguages.deltas.solidity

import miksilo.modularLanguages.core.bigrammar.{BiGrammar, BiGrammarToParser}
import miksilo.modularLanguages.core.bigrammar.grammars.{Keyword, StringGrammar, StringLiteral}
import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.core.node.{GrammarKey, NodeShape}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.modularLanguages.deltas.classes.HasConstraintsDelta
import miksilo.modularLanguages.deltas.statement.StatementDelta

object InlineAssemblyStatementDelta extends DeltaWithGrammar with HasConstraintsDelta {
  override def description = "Adds the inline assembly statement"

  object Shape extends NodeShape
  object AssemblyItem extends GrammarKey
  object AssemblyExpression extends GrammarKey
  override def transformGrammars(_grammars: LanguageGrammars, language: Language): Unit = {
    val grammars = _grammars
    import grammars._
    val assemblyIdentifierOrList: BiGrammar = identifier | identifier.toParameterList
    val assemblyExpression = create(AssemblyExpression)
    val assemblyCall: BiGrammar = (identifier | "return" | Keyword("address", reserved = false) | "byte") ~
      assemblyExpression.toParameterList.option
    assemblyExpression.addAlternative(assemblyCall)
    val hexCharacter = grammars.regexGrammar("""[0-9A-Fa-f]""".r, "hex character")
    val hexPair = hexCharacter ~ hexCharacter
    val hexLiteral: BiGrammar = "hex" ~~ ("\"" ~~ hexPair.many ~~ "\"" | "'" ~~ hexPair.many ~~ "'")

    val hexNumber: BiGrammar = "0" ~ ("x" | "X") ~ hexCharacter.some

    val decimalLiteral: BiGrammar = new StringGrammar {
      override def getParserBuilder(keywords: collection.Set[String]) = BiGrammarToParser.floatingPointNumber
    }
    val assemblyLiteral: BiGrammar = StringLiteral | hexLiteral | decimalLiteral | hexNumber
    assemblyExpression.addAlternative(assemblyLiteral)
    val assemblyLocalDefinition: BiGrammar = "let" ~~ assemblyIdentifierOrList ~ (":=" ~> assemblyExpression).option
    val assemblyAssignment: BiGrammar = assemblyIdentifierOrList ~ ":=" ~ assemblyExpression
    val assemblyStackAssignment: BiGrammar = "=:" ~ identifier
    val labelDefinition: BiGrammar = identifier ~ ":"

    val breakKeyword: BiGrammar = "break"
    val continueKeyword: BiGrammar = "continue"
    val assemblyItem = create(AssemblyItem, identifier |
      assemblyExpression |
      assemblyLocalDefinition |
      assemblyAssignment |
      assemblyStackAssignment | labelDefinition |
      breakKeyword | continueKeyword | number | StringLiteral | hexLiteral)
    val assemblyBlock = "{" ~> assemblyItem.manyVertical ~< "}"
    val subAssembly: BiGrammar = "assembly" ~~ identifier ~~ assemblyBlock
    assemblyItem.addAlternative(subAssembly)

    val assemblyIf: BiGrammar = "if" ~~ assemblyExpression ~~ assemblyBlock
    assemblyItem.addAlternative(assemblyIf)
    val assemblyFor: BiGrammar = "for" ~~ (assemblyBlock | assemblyExpression) ~~
      assemblyExpression ~~
      (assemblyBlock | assemblyExpression) ~~
      assemblyBlock
    assemblyItem.addAlternative(assemblyFor)

    val assemblyFunctionDefinition: BiGrammar = "function" ~~ identifier ~~ identifier.toParameterList ~
      ("->" ~ identifier.manySeparated(stringToGrammar(",") ~ printSpace)).option ~
      assemblyBlock
    assemblyItem.addAlternative(assemblyFunctionDefinition)

    val assemblyCase = "case" ~~ assemblyLiteral ~~ assemblyBlock | "default" ~~ assemblyBlock
    val assemblySwitch: BiGrammar = "switch" ~~ assemblyExpression ~~ assemblyCase.manyVertical
    assemblyItem.addAlternative(assemblySwitch)

    assemblyItem.addAlternative(assemblyBlock)
    val grammar = "assembly" ~~ StringLiteral.option ~ assemblyBlock asNode Shape
    find(StatementDelta.Grammar).addAlternative(grammar)
  }

  override def dependencies = Set(StatementDelta)

  override def shape = Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {

  }
}
