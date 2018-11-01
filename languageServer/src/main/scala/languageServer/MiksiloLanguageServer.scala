package languageServer

import com.typesafe.scalalogging.LazyLogging
import core.deltas.path.NodePath
import core.language.exceptions.BadInputException
import core.language.node.{FilePosition, FileRange, NodeLike, UriEntrance}
import core.language.{Compilation, Language, SourceElement}
import core.smarts.Proofs
import core.smarts.objects.NamedDeclaration
import langserver.types._
import languageServer.lsp._

class MiksiloLanguageServer(val language: Language) extends LanguageServer
  with DefinitionProvider
  with ReferencesProvider
  with CompletionProvider
  with LazyLogging {

  var client: LanguageClient = _
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
      val diagnostics = getCompilation.diagnosticsForFile(parameters.textDocument.uri)
      client.sendDiagnostics(PublishDiagnostics(parameters.textDocument.uri, diagnostics))
    }
  }

  def compile(): Unit = {
    val compilation = new Compilation(language, documentManager, Some(currentDocumentId.uri))
    this.compilation = Some(compilation)
    try {
      compilation.runPhases()
    } catch {
      case e: BadInputException => //TODO move to diagnostics.
        logger.debug(e.toString)
    }
  }

  def getCompilation: Compilation = {
    if (compilation.isEmpty)
      compile()
    compilation.get
  }

  def getProofs: Option[Proofs] = {
    Option(getCompilation.proofs)
  }

  def getSourceElement(position: FilePosition): SourceElement = {
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
      val childPosition = childPositions.find(kv => kv.filePosition.exists(r => r.contains(position)))
      childPosition.fold[SourceElement](node)(x => x)
    }
    getForNode(getCompilation.root)
  }

  def getSourceElementForNode(path: NodePath, position: FilePosition): Option[SourceElement] = {
    val maybeUri = path.get(UriEntrance)
    val doesContain = maybeUri match {
      case Some(uri) => position.uri == uri
      case None => true
    }
    if (!doesContain)
      return None

    val childResults = path.dataView.flatMap(t => {
      val childrenForField = NodeLike.getChildNodeLikes[NodePath](t._2)
      childrenForField.flatMap(child => getSourceElementForNode(child, position).toSeq)
    })

    Some(childResults.headOption.getOrElse(path))
  }

  override def initialize(parameters: InitializeParams): Unit = {}

  override def initialized(): Unit = {}

  override def gotoDefinition(parameters: DocumentPosition): Seq[Location] = {
    currentDocumentId = parameters.textDocument
    logger.debug("Went into gotoDefinition")
    val location = for {
      proofs <- getProofs
      element = getSourceElement(FilePosition(parameters.textDocument.uri, parameters.position))
      definition <- proofs.gotoDefinition(element)
      fileRange <- definition.origin.flatMap(o => o.filePosition)
    } yield Location(fileRange.uri, new langserver.types.Range(fileRange.range.start, fileRange.range.end)) //TODO misschien de Types file kopieren en Location vervangen door FileRange?
    location.toSeq
  }

  override def complete(params: DocumentPosition): CompletionList = {
    currentDocumentId = params.textDocument
    val position = params.position
    logger.debug("Went into complete")
    val completions: Seq[CompletionItem] = for {
      proofs <- getProofs.toSeq
      scopeGraph = proofs.scopeGraph
      element = getSourceElement(FilePosition(params.textDocument.uri, position))
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
      element = getSourceElement(FilePosition(parameters.textDocument.uri, parameters.position))
      definition <- getDefinitionFromDefinitionOrReferencePosition(proofs, element)
    } yield {

      val referencesRanges = for {
        references <- proofs.findReferences(definition)
        range <- references.origin.flatMap(e => e.filePosition).toSeq
      } yield range

      var fileRanges: Seq[FileRange] = referencesRanges
      if (parameters.context.includeDeclaration)
        fileRanges = definition.origin.flatMap(o => o.filePosition).toSeq ++ fileRanges

      fileRanges.map(fileRange => Location(fileRange.uri, new langserver.types.Range(fileRange.range.start, fileRange.range.end)))
    }
    maybeResult.getOrElse(Seq.empty)
  }

  override def setClient(client: LanguageClient): Unit = {
    this.client = client
  }
}