package miksilo.lspprotocol.lsp

import miksilo.editorParser.parsers.editorParsers.Position

case class FileOffset(uri: String, offset: Int)
case class FilePosition(uri: String, position: Position)
