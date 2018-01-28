package core.language

import core.deltas.node.Node
import core.nabl.BindingsAndTypes

import scala.util.parsing.input.Position

trait GotoDefinition extends Capability {

  def apply(server: LanguageServer, position: Position): SourceElement = {
    val bindingsAndTypes = getBindingsAndTypes(server)

    val node: Node = server.findNodeAt(position)
    val reference = bindingsAndTypes.scopes.findReference(???)
    val declaration = bindingsAndTypes.scopes.resolve(reference)
    declaration.origin
  }

  def getBindingsAndTypes(server: LanguageServer): BindingsAndTypes = {
    server.state.getOrElseUpdate(this, {
      ???
    }).asInstanceOf[BindingsAndTypes]
  }
}
