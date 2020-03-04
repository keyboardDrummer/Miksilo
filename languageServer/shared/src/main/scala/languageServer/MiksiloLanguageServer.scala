package languageServer

import core.LazyLogging
import core.language.exceptions.BadInputException
import core.language.{Compilation, CompilationCache, Language, NotStarted, SourceElement}
import core.parsers.core.ParseText
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
  val compilationCache: CompilationCache = new CompilationCache(language, documentManager)
  var compilation: Compilation = _

  override def textDocumentSync = TextDocumentSyncKind.Incremental

  override def didOpen(parameters: TextDocumentItem): Unit = {
    compilation = new Compilation(compilationCache, Some(parameters.uri))
    documentManager.onOpenTextDocument(parameters)
    sendDiagnostics()
  }

  override def didClose(parameters: TextDocumentIdentifier): Unit = documentManager.onCloseTextDocument(parameters)

  override def didSave(parameters: DidSaveTextDocumentParams): Unit = {}

  override def didChange(parameters: DidChangeTextDocumentParams): Unit = {
    compilation = new Compilation(compilationCache, Some(parameters.textDocument.uri))
    if (parameters.contentChanges.nonEmpty) {
      documentManager.onChangeTextDocument(parameters.textDocument, parameters.contentChanges)
    }
    sendDiagnostics()
  }

  private def sendDiagnostics(): Unit = {
    if (client != null) {
      val uri = compilation.rootFile.get
      val diagnostics = getCompilation.diagnosticsForFile(uri)
      logger.info("Sending diagnostics: " + diagnostics)
      client.sendDiagnostics(PublishDiagnostics(uri, diagnostics))
    }
  }

  def compile(): Unit = {
    try {
      compilation.runPhases()
    } catch {
      case e: BadInputException => //TODO move to diagnostics.
        logger.debug(e.toString)
    }
  }

  def getCompilation: Compilation = {
    if (!compilation.isStarted)
      compile()
    compilation
  }

  def getProofs: Option[Proofs] = {
    Option(getCompilation.proofs)
  }

  def getSourceElement(text: ParseText, position: FilePosition): Option[SourceElement] = {
    val fileOffset = FileOffset(position.uri, text.getOffset(position.position))
    getCompilation.program.getChildForPosition(fileOffset)
  }

  override def initialize(parameters: InitializeParams): Unit = {}

  override def initialized(): Unit = {}

  override def gotoDefinition(parameters: DocumentPosition): Seq[FileRange] = {
    logger.debug("Went into gotoDefinition")
    val text: ParseText = documentManager.getFileParseText(parameters.textDocument.uri)
    val fileRange = for {
      proofs <- getProofs
      element <- getSourceElement(text, FilePosition(parameters.textDocument.uri, parameters.position))
      definition <- proofs.gotoDefinition(element)
      fileRange <- definition.origin.flatMap(o => o.fileRange.map(fr => FileRange.fromOffsetRange(text, fr)))
    } yield fileRange //TODO misschien de Types file kopieren en Location vervangen door FileRange?
    fileRange.toSeq
  }

  override def complete(params: DocumentPosition): CompletionList = {
    val text: ParseText = documentManager.getFileParseText(params.textDocument.uri)
    val position = params.position
    val offset = text.getOffset(position)
    logger.debug("Went into complete")
    val completions: Seq[CompletionItem] = for {
      proofs <- getProofs.toSeq
      scopeGraph = proofs.scopeGraph
      element <- getSourceElement(text, FilePosition(params.textDocument.uri, position)).toSeq
      reference <- scopeGraph.getReferenceFromSourceElement(element).toSeq
      prefixLength = offset - reference.origin.get.range.get.from
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

  override def references(parameters: ReferencesParams): collection.Seq[FileRange] = {
    logger.debug("Went into references")
    val text: ParseText = documentManager.getFileParseText(parameters.textDocument.uri)
    val maybeResult = for {
      proofs <- getProofs
      element <- getSourceElement(text, FilePosition(parameters.textDocument.uri, parameters.position))
      definition <- getDefinitionFromDefinitionOrReferencePosition(proofs, element)
    } yield {

      val referencesRanges: collection.Seq[FileRange] = for {
        references <- proofs.findReferences(definition)
        range <- references.origin.flatMap(e => e.fileRange.map(FileRange.fromOffsetRange(text, _))).toSeq
      } yield range

      var fileRanges: collection.Seq[FileRange] = referencesRanges
      if (parameters.context.includeDeclaration)
        fileRanges = definition.origin.flatMap(o => o.fileRange.map(FileRange.fromOffsetRange(text, _))).toSeq ++ fileRanges

      fileRanges
    }
    maybeResult.getOrElse(Seq.empty)
  }

  override def setClient(client: LanguageClient): Unit = {
    this.client = client
    compilationCache.metrics = client.trackMetric
  }

  override def documentSymbols(params: DocumentSymbolParams): Seq[SymbolInformation] = {
    val proofs = getCompilation.proofs
    if (proofs == null)
      return Seq.empty

    val text = documentManager.getFileParseText(params.textDocument.uri)

    val declarations = getCompilation.proofs.scopeGraph.declarationsPerFile.getOrElse(params.textDocument.uri, Seq.empty).toSeq
    declarations.
      filter(declaration => declaration.name.nonEmpty && {
        if (declaration.origin.isEmpty) {
          logger.error(s"[BUG] Empty origin for declaration ${declaration.name}")
          false
        } else if (declaration.origin.get.fileRange.isEmpty) {
          logger.error(s"[BUG] Empty fileRange for declaration ${declaration.name}")
          false
        } else {
          true
        }
      }).
      map(declaration => {
        val fileRange = FileRange.fromOffsetRange(text, declaration.origin.get.fileRange.get)
        SymbolInformation(declaration.name, SymbolKind.Variable, fileRange, None)
      })
  }

  override def rename(params: RenameParams): WorkspaceEdit = {
    val locations = references(ReferencesParams(params.textDocument, params.position, ReferenceContext(true)))
    WorkspaceEdit(locations.groupBy(l => l.uri).map(t => {
      (t._1, t._2.map(r => TextEdit(r.range, params.newName)))
    }))
  }

  override def getCodeActions(parameters: CodeActionParams): Seq[CodeAction] = {
    val diagnostics = parameters.context.diagnostics.map(d => d.identifier).toSet
    val compilation = getCompilation
    compilation.fixesPerDiagnostics.
      filter(entry => diagnostics.contains(entry._1)).flatMap(entry => entry._2).toSeq
  }
}