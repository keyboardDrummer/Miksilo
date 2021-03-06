package miksilo.languageServer.server

import miksilo.editorParser.LazyLogging
import miksilo.languageServer.core.language.Language
import miksilo.lspprotocol.jsonRpc.{JsonRpcConnection, SerialWorkQueue, WorkItem}
import miksilo.lspprotocol.lsp.{LanguageServer, SharedLSPServer}

import scala.util.Try

trait LanguageBuilder {
  def key: String
  def build(arguments: collection.Seq[String]): LanguageServer
}

case class SimpleLanguageBuilder(key: String, language: Language) extends LanguageBuilder {

  override def build(arguments: collection.Seq[String]) = new MiksiloLanguageServer(language)
}

class LanguageServerMain(builders: Seq[LanguageBuilder],
                         connection: JsonRpcConnection,
                         workQueue: SerialWorkQueue[WorkItem]) extends LazyLogging {

  val languageMap = builders.map(l => (l.key, l)).toMap

  def main(args: Array[String]): Unit = {
    val serverOption = getServer(args)
    serverOption.foreach(server => {
      logger.debug(s"Starting server in ${System.getenv("PWD")}")
      val lspServer = Try {
        val languageServer = server
        new SharedLSPServer(languageServer, connection, workQueue)
      }
      lspServer.recover{case e => logger.error(e.getMessage); e.printStackTrace() }
      connection.listen()
    })
  }

  def getServer(args: collection.Seq[String]): Option[LanguageServer] = {
    if (builders.size == 1) {
      Some(builders.head.build(args))
    } else {
      def error = s"Please specify with which language to run Miksilo. You can choose from: ${builders.map(b => b.key).mkString(", ")}"
      if (args.isEmpty) {
        logger.error(error)
        return None
      }

      val languageOption = languageMap.get(args.head)
      languageOption match {
        case None =>
          logger.error(error)
          None
        case Some(languageBuilder) =>
          Some(languageBuilder.build(args.drop(1)))
      }
    }
  }
}
