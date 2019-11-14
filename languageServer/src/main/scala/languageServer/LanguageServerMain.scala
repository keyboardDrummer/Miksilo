package languageServer

import ch.qos.logback.classic.{Level, Logger}
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.ConsoleAppender
import ch.qos.logback.core.encoder.LayoutWrappingEncoder
import ch.qos.logback.core.layout.EchoLayout
import com.typesafe.scalalogging.LazyLogging
import core.language.Language
import jsonRpc.JsonRpcConnection
import lsp.LSPServer
import org.slf4j.LoggerFactory

import scala.util.Try

trait LanguageBuilder {
  def key: String
  def build(arguments: Seq[String]): Language
}

class LanguageServerMain(builders: Seq[LanguageBuilder]) extends LazyLogging {

  val languageMap = builders.map(l => (l.key, l)).toMap

  def main(args: Array[String]): Unit = {

    val innerLogger = LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[Logger]
    innerLogger.setLevel(Level.INFO)
    sendAllLoggingToStdErr(innerLogger)

    val languageOption = getLanguage(args)
    languageOption.foreach(language => {
      logger.debug(s"Starting server in ${System.getenv("PWD")}")
      val connection = new JsonRpcConnection(System.in, System.out)
      val lspServer = Try {
        val languageServer = new MiksiloLanguageServer(language)
        new LSPServer(languageServer, connection)
      }
      lspServer.recover{case e => logger.error(e.getMessage); e.printStackTrace() }
      connection.listen()
    })
  }

  def getLanguage(args: Array[String]): Option[Language] = {
    if (builders.size == 1) {
      Some(builders.head.build(args))
    } else {
      if (args.isEmpty) {
        logger.debug("Please specify with which language to run Miksilo")
        return None
      }

      val languageOption = languageMap.get(args.head)
      languageOption match {
        case None =>
          logger.debug("Please specify with which language to run Miksilo")
          None
        case Some(languageBuilder) =>
          Some(languageBuilder.build(args.drop(1)))
      }
    }
  }

  private def sendAllLoggingToStdErr(innerLogger: Logger): Unit = {
    innerLogger.detachAndStopAllAppenders()
    val consoleAppender = new ConsoleAppender[ILoggingEvent]()
    consoleAppender.setContext(innerLogger.getLoggerContext)
    consoleAppender.setTarget("System.err")
    val encoder = new LayoutWrappingEncoder[ILoggingEvent]
    encoder.setLayout(new EchoLayout[ILoggingEvent])
    consoleAppender.setEncoder(encoder)
    consoleAppender.start()
    innerLogger.addAppender(consoleAppender)
  }
}
