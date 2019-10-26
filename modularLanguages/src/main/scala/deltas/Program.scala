package deltas

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{Level, Logger}
import ch.qos.logback.core.ConsoleAppender
import ch.qos.logback.core.encoder.LayoutWrappingEncoder
import ch.qos.logback.core.layout.EchoLayout
import com.typesafe.scalalogging.LazyLogging
import core.language.Language
import deltas.cloudformation.CloudFormationLanguage
import deltas.javac.JavaLanguage
import deltas.smithy.SmithyLanguage
import deltas.solidity.SolidityLanguage
import deltas.verilog.VerilogLanguage
import jsonRpc.JsonRpcConnection
import languageServer.MiksiloLanguageServer
import lsp.LSPServer
import org.slf4j.LoggerFactory

import scala.reflect.io.File
import scala.util.Try

trait LanguageBuilder {
  def key: String
  def build(arguments: Seq[String]): Language
}

class CloudFormationLanguageBuilder(json: Boolean = true) extends LanguageBuilder with LazyLogging {
  override def build(arguments: Seq[String]) = {
    val resourceSpecificationOption = if (arguments.isEmpty) {
      logger.debug("CloudFormation language requires passing a path to a resource specification as an argument")
      None
    } else {
      val path = arguments.head
      val inputStream = File(path).inputStream()
      Some(inputStream)
    }
    val cloudFormation = new CloudFormationLanguage(resourceSpecificationOption)
    if (json) cloudFormation.jsonLanguage else cloudFormation.yamlLanguage
  }

  override def key = if (json) "cloudFormation" else "yamlCloudFormation"
}

object VerilogLanguageBuilder extends LanguageBuilder {
  override def key = "verilog"
  override def build(arguments: Seq[String]) = VerilogLanguage.language
}

object JavaLanguageBuilder extends LanguageBuilder {
  override def key = "java"

  override def build(arguments: Seq[String]) = JavaLanguage.java
}

object SolidityLanguageBuilder extends LanguageBuilder {
  override def key = "solidity"

  override def build(arguments: Seq[String]) = SolidityLanguage.language
}

object SmithyLanguageBuilder extends LanguageBuilder {
  override def key = "smithy"

  override def build(arguments: Seq[String]) = SmithyLanguage.language
}

object Program extends LazyLogging {

  val languages: Seq[LanguageBuilder] = Seq(
    new CloudFormationLanguageBuilder(json = true),
    new CloudFormationLanguageBuilder(json = false),
    VerilogLanguageBuilder,
    JavaLanguageBuilder,
    SolidityLanguageBuilder,
    SmithyLanguageBuilder
  )
  val languageMap = languages.map(l => (l.key, l)).toMap

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      logger.debug("Please specify with which language to run Miksilo")
      return
    }

    val remainingArguments = args.drop(1)

    val innerLogger = LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[Logger]
    innerLogger.setLevel(Level.INFO)

    sendAllLoggingToStdErr(innerLogger)
    logger.debug(s"Starting server in ${System.getenv("PWD")}")

    val languageOption = languageMap.get(args.head)
    languageOption match {
      case None =>
        logger.debug("Please specify with which language to run Miksilo")
      case Some(languageBuilder) =>
        val connection = new JsonRpcConnection(System.in, System.out)
        val lspServer = Try {
          val languageServer = new MiksiloLanguageServer(languageBuilder.build(remainingArguments))
          new LSPServer(languageServer, connection)
        }
        lspServer.recover{case e => logger.error(e.getMessage); e.printStackTrace() }
        connection.listen()
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

