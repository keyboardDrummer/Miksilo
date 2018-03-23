package lsp

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import core.deltas.path.{NodePath, PathRoot}
import core.language.exceptions.BadInputException
import core.language.node.NodeLike
import core.language.{Compilation, Language, SourceElement}
import core.smarts.{Proofs, SolveConstraintsDelta}
import langserver.core.TextDocument
import langserver.types._

object HumanPosition {
  implicit def toPosition(position: HumanPosition): Position = new Position(position.line - 1, position.character - 1)
  implicit def fromPosition(position: Position): HumanPosition = new HumanPosition(position.line + 1, position.character + 1)
}

case class HumanPosition(line: Int, character: Int) {}

class MiksiloLanguageServer(val language: Language, connection: Connection)
  extends LanguageServer(connection) with GotoProvider with CompletionProvider {

  private val constraintsPhaseIndex = language.compilerPhases.indexWhere(p => p.key == SolveConstraintsDelta)
  private val proofPhases = language.compilerPhases.take(constraintsPhaseIndex + 1)
  var currentDocumentId: TextDocumentIdentifier = _
  var compilation: Option[Compilation] = None

  override def onChangeTextDocument(td: VersionedTextDocumentIdentifier, changes: Seq[TextDocumentContentChangeEvent]): Unit = {
    compilation = None
    super.onChangeTextDocument(td, changes)
  }

  def compile(): Unit = {
    val compilation = new Compilation(language)
    try {
      val input = getInputStreamFromDocument(currentDocument)
      compilation.program = language.parse(input).get
      for(phase <- proofPhases)
        phase.action(compilation)
      this.compilation = Some(compilation)
    } catch {
      case e: BadInputException =>
        logger.debug(e.toString)
    }
  }

  private def getInputStreamFromDocument(document: langserver.core.TextDocument) = {
    new ByteArrayInputStream(new String(document.contents).getBytes(StandardCharsets.UTF_8))
  }

  private def currentDocument: TextDocument = {
    documentManager.documentForUri(currentDocumentId.uri).getOrElse({
      val contents = scala.io.Source.fromFile(currentDocumentId.uri.drop(7)).mkString
      TextDocument(currentDocumentId.uri, contents.toCharArray)
    })
  }

  def getCompilation: Option[Compilation] = {
    if (compilation.isEmpty)
      compile()
    compilation
  }

  def getProofs: Option[Proofs] = {
    getCompilation.map(c => c.proofs)
  }

  def getSourceElement(position: Position): SourceElement = {
    def getForNode(node: NodePath): SourceElement = {
      val childPositions = node.dataView.flatMap(kv => {
        val value = kv._2
        val childPaths = NodeLike.getChildNodeLikes[NodePath](value)
        if (childPaths.isEmpty) {
          Seq(node.getLocation(kv._1))
        } else {
          childPaths.map(child => getForNode(child))
        }
      })
      val childPosition = childPositions.find(kv => kv.position.exists(r => r.contains(position)))
      childPosition.fold[SourceElement](node)(x => x)
    }
    getForNode(PathRoot(getCompilation.get.program))
  }

  override def gotoDefinitionRequest(textDocument: TextDocumentIdentifier, position: Position): DefinitionResult = {
    currentDocumentId = textDocument
    logger.debug("Went into gotoDefinitionRequest")
    val location = for {
      proofs <- getProofs
      element = getSourceElement(position)
      declaration <- proofs.resolveLocation(element)
      range <- declaration.position
    } yield Location(textDocument.uri, new langserver.types.Range(range.start, range.end))
    DefinitionResult(location.toSeq)
  }

  override def getOptions: CompletionOptions = CompletionOptions(resolveProvider = false, Seq.empty)

  override def completionRequest(textDocument: TextDocumentIdentifier, position: Position): CompletionList = {
    currentDocumentId = textDocument
    logger.debug("Went into completionRequest")
    val completions: Seq[CompletionItem] = for {
      proofs <- getProofs.toSeq
      scopeGraph = proofs.scopeGraph
      element = getSourceElement(position)
      reference <- scopeGraph.findReference(element).toSeq
      prefixLength = position.character - reference.origin.get.position.get.start.character
      prefix = reference.name.take(prefixLength)
      declaration <- scopeGraph.resolveWithoutNameCheck(reference).
        filter(declaration => declaration.origin.nonEmpty).
        filter(declaration => declaration.name.startsWith(prefix))
      missingText = declaration.name.drop(prefixLength)
      completion = CompletionItem(declaration.name, kind = Some(CompletionItemKind.Text), insertText = Some(missingText))
    } yield completion

    CompletionList(isIncomplete = false, completions)
  }
}
