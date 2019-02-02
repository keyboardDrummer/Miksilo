package languageServer

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{Level, Logger}
import ch.qos.logback.core.ConsoleAppender
import ch.qos.logback.core.encoder.LayoutWrappingEncoder
import ch.qos.logback.core.layout.EchoLayout
import com.typesafe.scalalogging.LazyLogging
import core.language.Language
import deltas.cloudformation.CloudFormationLanguage
import deltas.javac.JavaLanguage
import deltas.solidity.SolidityLanguage
import deltas.verilog.VerilogLanguage
import languageServer.lsp.{JsonRpcConnection, LSPServer}
import org.slf4j.LoggerFactory

import scala.util.Try

object Program extends LazyLogging {

  val languages: Map[String, Language] = Map(
    "cloudFormation" -> CloudFormationLanguage.language,
    "verilog" -> VerilogLanguage.language,
    "java" -> JavaLanguage.java,
    "solidity" -> SolidityLanguage.language,
  )

  def main(args: Array[String]): Unit = {

    val innerLogger = LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[Logger]
    innerLogger.setLevel(Level.INFO)

    sendAllLoggingToStdErr(innerLogger)
    logger.debug(s"Starting server in ${System.getenv("PWD")}")

    val languageNameOption = args.headOption
    val language = languageNameOption.flatMap(languageName => languages.get(languageName)).getOrElse(languages.values.head)
    val connection = new JsonRpcConnection(System.in, System.out)
    val lspServer = Try {
      val languageServer = new MiksiloLanguageServer(language)
      new LSPServer(languageServer, connection)
    }
    lspServer.recover{case e => logger.error(e.getMessage); e.printStackTrace() }
    connection.listen()
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

