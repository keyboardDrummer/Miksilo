package languageServer

import com.typesafe.scalalogging.LazyLogging
import core.language.exceptions.BadInputException
import core.language.{Compilation, Language, SourceElement}
import core.parsers.editorParsers.TextEdit
import core.smarts.Proofs
import core.smarts.objects.NamedDeclaration
import lsp._

class MiksiloLanguageServer(val language: Language) extends LanguageServer
  with DefinitionProvider
  with ReferencesProvider
  with CompletionProvider
  with DocumentSymbolProvider
  with RenameProvider
  with CodeActionProvider
  with LazyLogging {

  var client: LanguageClient = _
  private val documentManager = new TextDocumentManager()
  var currentDocumentId: TextDocumentIdentifier = _
  var compilation: Option[Compilation] = None

  override def textDocumentSync = TextDocumentSyncKind.Incremental

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

  def getSourceElement(position: FilePosition): Option[SourceElement] = {
    getCompilation.program.getChildForPosition(position)
  }

  override def initialize(parameters: InitializeParams): Unit = {}

  override def initialized(): Unit = {}

  override def gotoDefinition(parameters: DocumentPosition): Seq[FileRange] = {
    currentDocumentId = parameters.textDocument
    logger.debug("Went into gotoDefinition")
    val fileRange = for {
      proofs <- getProofs
      element <- getSourceElement(FilePosition(parameters.textDocument.uri, parameters.position))
      definition <- proofs.gotoDefinition(element)
      fileRange <- definition.origin.flatMap(o => o.fileRange)
    } yield fileRange //TODO misschien de Types file kopieren en Location vervangen door FileRange?
    fileRange.toSeq
  }

  override def complete(params: DocumentPosition): CompletionList = {
    currentDocumentId = params.textDocument
    val position = params.position
    logger.debug("Went into complete")
    val completions: Seq[CompletionItem] = for {
      proofs <- getProofs.toSeq
      scopeGraph = proofs.scopeGraph
      element <- getSourceElement(FilePosition(params.textDocument.uri, position)).toSeq
      reference <- scopeGraph.getReferenceFromSourceElement(element).toSeq
      prefixLength = position.character - reference.origin.get.range.get.start.character
      prefix = reference.name.take(prefixLength)
      declaration <- scopeGraph.resolveWithoutNameCheck(reference).
        filter(declaration => declaration.name.startsWith(prefix))
      insertText = declaration.name
      completion = CompletionItem(declaration.name, kind = Some(CompletionItemKind.Variable), insertText = Some(insertText))
    } yield completion

    CompletionList(isIncomplete = false, completions)
  }

  override def getOptions: CompletionOptions = CompletionOptions(resolveProvider = false, Seq.empty)

  def getDefinitionFromDefinitionOrReferencePosition(proofs: Proofs, element: SourceElement): Option[NamedDeclaration] = {
    proofs.scopeGraph.findDeclaration(element).orElse(proofs.gotoDefinition(element))
  }

  override def references(parameters: ReferencesParams): Seq[FileRange] = {
    currentDocumentId = parameters.textDocument
    logger.debug("Went into references")
    val maybeResult = for {
      proofs <- getProofs
      element <- getSourceElement(FilePosition(parameters.textDocument.uri, parameters.position))
      definition <- getDefinitionFromDefinitionOrReferencePosition(proofs, element)
    } yield {

      val referencesRanges = for {
        references <- proofs.findReferences(definition)
        range <- references.origin.flatMap(e => e.fileRange).toSeq
      } yield range

      var fileRanges: Seq[FileRange] = referencesRanges
      if (parameters.context.includeDeclaration)
        fileRanges = definition.origin.flatMap(o => o.fileRange).toSeq ++ fileRanges

      fileRanges
    }
    maybeResult.getOrElse(Seq.empty)
  }

  override def setClient(client: LanguageClient): Unit = {
    this.client = client
  }

  override def documentSymbols(params: DocumentSymbolParams): Seq[SymbolInformation] = {
    currentDocumentId = params.textDocument
    val proofs = getCompilation.proofs
    if (proofs == null)
      return Seq.empty

    val declarations = getCompilation.proofs.scopeGraph.declarationsPerFile.getOrElse(params.textDocument.uri, Seq.empty).toSeq
    declarations.
      filter(declaration => declaration.name.nonEmpty).
      map(declaration => SymbolInformation(declaration.name, SymbolKind.Variable, declaration.origin.get.fileRange.get, None))
  }

  override def rename(params: RenameParams): WorkspaceEdit = {
    val locations = references(ReferencesParams(params.textDocument, params.position, ReferenceContext(true)))
    WorkspaceEdit(locations.groupBy(l => l.uri).map(t => {
      (t._1, t._2.map(r => TextEdit(r.range, params.newName)))
    }))
  }

  override def getCodeActions(parameters: CodeActionParams): Seq[CodeAction] = {
    currentDocumentId = parameters.textDocument

    val diagnostics = parameters.context.diagnostics.map(d => d.identifier).toSet
    val compilation = getCompilation
    compilation.fixesPerDiagnostics.
      filter(entry => diagnostics.contains(entry._1)).flatMap(entry => entry._2).toSeq
  }
}