package core.language.node

import languageServer.{Position, SourceRange}


case class FilePosition(uri: String, position: Position)
