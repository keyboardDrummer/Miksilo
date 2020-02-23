package lsp

import core.parsers.editorParsers.Position

case class FileOffset(uri: String, offset: Int)
case class FilePosition(uri: String, position: Position)
