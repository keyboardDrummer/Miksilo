package languageServer

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import com.typesafe.scalalogging.LazyLogging
import core.deltas.path.{NodePath, PathRoot}
import core.language.exceptions.BadInputException
import core.language.node.{NodeLike, SourceRange}
import core.language.{Compilation, Language, SourceElement}
import core.smarts.objects.NamedDeclaration
import core.smarts.{Proofs, SolveConstraintsDelta}
import langserver.core.TextDocument
import langserver.types._
import languageServer.lsp._

class MiksiloLanguageServer(val language: Language) extends LanguageServer
  with DefinitionProvider
  with ReferencesProvider
  with CompletionProvider
  with LazyLogging {

  var client: LanguageClient = _
  private val constraintsPhaseIndex = language.compilerPhases.indexWhere(p => p.key == SolveConstraintsDelta)
  private val proofPhases = language.compilerPhases.take(constraintsPhaseIndex + 1)
  private val documentManager = new TextDocumentManager()
  var currentDocumentId: TextDocumentIdentifier = _
  var compilation: Option[Compilation] = None

  override def didOpen(parameters: TextDocumentItem): Unit = documentManager.onOpenTextDocument(parameters)

  override def didClose(parameters: TextDocumentIdentifier): Unit = documentManager.onCloseTextDocument(parameters)

  override def didSave(parameters: DidSaveTextDocumentParams): Unit = {}

  override def didChange(parameters: DidChangeTextDocumentParams): Unit = {
    compilation = None
    if (parameters.contentChanges.nonEmpty)
      documentManager.onChangeTextDocument(parameters.textDocument, parameters.contentChanges)
    if (client != null) {
      currentDocumentId = TextDocumentIdentifier(parameters.textDocument.uri)
      client.sendDiagnostics(PublishDiagnostics(parameters.textDocument.uri, getCompilation.diagnostics))
    }
  }

  def compile(): Unit = {
    val compilation = new Compilation(language)
    this.compilation = Some(compilation)
    try {
      val input = getInputStreamFromDocument(currentDocument)
      language.parseIntoDiagnostic(input) match {
        case Left(program) =>

          compilation.program = program
          for(phase <- proofPhases)
            phase.action(compilation)

          compilation.diagnostics ++= compilation.remainingConstraints.flatMap(constraint => constraint.getDiagnostic.toSeq)
        case Right(diagnostics) =>
          compilation.diagnostics ++= diagnostics
      }

    } catch {
      case e: BadInputException => //TODO move to diagnostics.
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

  def getCompilation: Compilation = {
    if (compilation.isEmpty)
      compile()
    compilation.get
  }

  def getProofs: Option[Proofs] = {
    Option(getCompilation.proofs)
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
    getForNode(PathRoot(getCompilation.program))
  }

  override def initialize(parameters: InitializeParams): Unit = {}

  override def initialized(): Unit = {}

  override def gotoDefinition(parameters: DocumentPosition): Seq[Location] = {
    currentDocumentId = parameters.textDocument
    logger.debug("Went into gotoDefinition")
    val location = for {
      proofs <- getProofs
      element = getSourceElement(parameters.position)
      definition <- proofs.gotoDefinition(element)
      range <- definition.origin.flatMap(o => o.position)
    } yield Location(parameters.textDocument.uri, new langserver.types.Range(range.start, range.end))
    location.toSeq
  }

  override def complete(params: DocumentPosition): CompletionList = {
    currentDocumentId = params.textDocument
    val position = params.position
    logger.debug("Went into complete")
    val completions: Seq[CompletionItem] = for {
      proofs <- getProofs.toSeq
      scopeGraph = proofs.scopeGraph
      element = getSourceElement(position)
      reference <- scopeGraph.findReference(element).toSeq
      prefixLength = position.character - reference.origin.get.position.get.start.character
      prefix = reference.name.take(prefixLength)
      declaration <- scopeGraph.resolveWithoutNameCheck(reference).
        filter(declaration => declaration.name.startsWith(prefix))
      missingText = declaration.name.drop(prefixLength)
      completion = CompletionItem(declaration.name, kind = Some(CompletionItemKind.Text), insertText = Some(missingText))
    } yield completion

    CompletionList(isIncomplete = false, completions)
  }

  override def getOptions: CompletionOptions = CompletionOptions(resolveProvider = false, Seq.empty)

  def getDefinitionFromDefinitionOrReferencePosition(proofs: Proofs, element: SourceElement): Option[NamedDeclaration] = {
    proofs.scopeGraph.findDeclaration(element).orElse(proofs.gotoDefinition(element))
  }

  override def references(parameters: ReferencesParams): Seq[Location] = {
    currentDocumentId = parameters.textDocument
    logger.debug("Went into references")
    val maybeResult = for {
      proofs <- getProofs
      element = getSourceElement(parameters.position)
      definition <- getDefinitionFromDefinitionOrReferencePosition(proofs, element)
    } yield {

      val referencesRanges = for {
        references <- proofs.findReferences(definition)
        range <- references.origin.flatMap(e => e.position).toSeq
      } yield range

      var positions: Seq[SourceRange] = referencesRanges
      if (parameters.context.includeDeclaration)
        positions = Seq(definition.origin.get.position.get) ++ positions

      positions.map(position => Location(parameters.textDocument.uri, new langserver.types.Range(position.start, position.end)))
    }
    maybeResult.getOrElse(Seq.empty)
  }

  override def setClient(client: LanguageClient): Unit = {
    this.client = client
  }
}
