package miksilo.lspprotocol.jsonRpc

import miksilo.editorParser.Logger
import scala.scalajs.js.Dynamic.{global => g}

object ConsoleLogger extends Logger {
  override def debug(message: String): Unit = {} //write(s"[DEBUG] $message\n")

  override def info(message: String): Unit = g.console.info(message)

  override def error(message: String): Unit = g.console.error(message)
}


