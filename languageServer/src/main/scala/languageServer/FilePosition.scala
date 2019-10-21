package languageServer

import core.parsers.editorParsers.Position

case class FilePosition(uri: String, position: Position)
