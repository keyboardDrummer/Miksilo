package core.language

import core.deltas.node.Node
import core.nabl.objects.Declaration
import core.nabl.scopes.ScopeGraph
import core.nabl.types.TypeGraph
import core.nabl.types.objects.Type

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

  case class BindingsAndTypes(scopes: ScopeGraph,
                         types: TypeGraph,
                         declarations: Map[Declaration, Type])
}
