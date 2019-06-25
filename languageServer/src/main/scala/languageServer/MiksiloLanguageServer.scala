package languageServer

import com.typesafe.scalalogging.LazyLogging
import core.deltas.path.NodePath
import core.language.exceptions.BadInputException
import core.language.node.{FilePosition, FileRange, NodeLike, SourceRange}
import core.language.{Compilation, Language, SourceElement}
import core.smarts.Proofs
import core.smarts.objects.NamedDeclaration
import langserver.types._
import languageServer.lsp._

class MiksiloLanguageServer(val language: Language) extends LanguageServer
  with DefinitionProvider
  with ReferencesProvider
  with CompletionProvider
  with DocumentSymbolProvider
  with RenameProvider
  with LazyLogging {

  var client: LanguageClient = _
  private val documentManager = new TextDocumentManager()
  var currentDocumentId: TextDocumentIdentifier = _
  var compilation: Option[Compilation] = None

  override def didOpen(parameters: TextDocumentItem): Unit = {
    compilation = None
    documentManager.onOpenTextDocument(parameters)
  }

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
    getSourceElementForNode(getCompilation.root, position).get
  }

  def getSourceElementForNode(element: SourceElement, filePosition:FilePosition):Option[SourceElement]={
    if (element.isOutsideFile(filePosition.uri))
      return None

    if(!element.range.exists(r => r.contains(filePosition.position)))
      return None

    element match {
      case path: NodePath =>
        val childResults = path.dataView.values.flatMap((fieldValue: Any) => {
          val childrenForField = getSourceElementsFromPath[NodePath](fieldValue)
          childrenForField.flatMap(child => getSourceElementForNode(child,filePosition).toSeq)
        })

        Some(childResults.headOption.getOrElse({
          element
        }))
      case _ => Some(element)
    }
  }

  def getSourceElementsFromPath[Self <: NodeLike](value: Any): Seq[SourceElement] = value match {
    case nodeLike: SourceElement =>
      Seq(nodeLike)
    case sequence: Seq[_] =>
      sequence.collect({ case nodeLikeChild: SourceElement => nodeLikeChild })
    case _ => Seq.empty
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
      fileRange <- definition.origin.flatMap(o => o.fileRange)
    } yield fileRangeToLocation(fileRange) //TODO misschien de Types file kopieren en Location vervangen door FileRange?
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
      prefixLength = position.character - reference.origin.get.range.get.start.character
      prefix = reference.name.take(prefixLength)
      declaration <- scopeGraph.resolveWithoutNameCheck(reference).
        filter(declaration => declaration.name.startsWith(prefix))
      insertText = declaration.name
      completion = CompletionItem(declaration.name, kind = Some(CompletionItemKind.Text), insertText = Some(insertText))
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
        range <- references.origin.flatMap(e => e.fileRange).toSeq
      } yield range

      var fileRanges: Seq[FileRange] = referencesRanges
      if (parameters.context.includeDeclaration)
        fileRanges = definition.origin.flatMap(o => o.fileRange).toSeq ++ fileRanges

      fileRanges.map(fileRange => fileRangeToLocation(fileRange))
    }
    maybeResult.getOrElse(Seq.empty)
  }

  private def fileRangeToLocation(fileRange: FileRange) = {
    Location(fileRange.uri, toLspRange(fileRange.range))
  }

  private def toLspRange(range: SourceRange): Range = {
    new langserver.types.Range(range.start, range.end)
  }

  override def setClient(client: LanguageClient): Unit = {
    this.client = client
  }

  override def documentSymbols(params: DocumentSymbolParams): Seq[SymbolInformation] = {
    currentDocumentId = params.textDocument
    val declarations = getCompilation.proofs.scopeGraph.declarationsPerFile.getOrElse(params.textDocument.uri, Seq.empty).toSeq
    declarations.map(declaration => SymbolInformation(declaration.name, SymbolKind.Variable, fileRangeToLocation(declaration.origin.get.fileRange.get), None))
  }

  override def rename(params: RenameParams): WorkspaceEdit = {
    val locations = references(ReferencesParams(params.textDocument, params.position, ReferenceContext(true)))
    WorkspaceEdit(locations.groupBy(l => l.uri).map(t => {
      (t._1, t._2.map(r => TextEdit(r.range, params.newName)))
    }))
  }
}