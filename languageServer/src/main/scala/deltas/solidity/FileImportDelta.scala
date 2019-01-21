package deltas.solidity

import core.bigrammar.grammars.StringLiteral
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.javac.classes.skeleton.HasConstraintsDelta
import deltas.solidity.MultiFileDelta.HasFileReferences

object FileImportDelta extends DeltaWithGrammar with HasConstraintsDelta {

  object Shape extends NodeShape
  object FileName extends NodeField
  object NewName extends NodeField

  implicit class FileImport[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def fileName: String = node.getValue(FileName).asInstanceOf[String]
    def fileName_=(value: String): Unit = node(FileName) = value

    def newName: String = node.getValue(FileName).asInstanceOf[String]
    def newName_=(value: String): Unit = node(FileName) = value
  }

  override def inject(language: Language): Unit = {
    MultiFileDelta.getFile.add(language, Shape, new HasFileReferences {
      override def getReferences(node: Node) = Seq(FileImport(node).fileName)
    })
    super.inject(language)
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val newName = create(NewName, (printSpace ~> "as" ~~> identifier).option.as(NewName))
    val simpleImport = "import" ~~ StringLiteral.as(FileName) ~~ (printSpace ~> "as" ~~> identifier).option.as(NewName) ~ ";" asNode Shape
    find(FileWithMembersDelta.Members).addAlternative(simpleImport)
  }

  override def description = "Adds importing an entire file"

  override def dependencies = Set(FileWithMembersDelta)

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    val fileImport: FileImport[NodePath] = path
    val fileDeclaration = builder.resolve(fileImport.fileName, fileImport.getSourceElement(FileName), parentScope, Some(FileWithMembersDelta.fileType))
    val fileScope = builder.getDeclaredScope(fileDeclaration)
    builder.importScope(parentScope, fileScope)
  }

  override def shape = Shape
}