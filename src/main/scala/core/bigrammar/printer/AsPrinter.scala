package core.bigrammar.printer

import core.bigrammar.WithMapG
import core.bigrammar.printer.TryState.{NodePrinter, State, fail}
import core.particles.node.NodeField
import core.responsiveDocument.ResponsiveDocument

import scala.util.Try

class AsPrinter(inner: NodePrinter, key: NodeField) extends NodePrinter {
  override def write(from: WithMapG[Any], state: State): Try[(State, ResponsiveDocument)] = {
    from.map.get(key).fold[Try[(State, ResponsiveDocument)]](
      fail(s"did not find as key $key in state ${from.map}"))(
      value => inner.write(WithMapG(value, from.map), state))
  }
}
