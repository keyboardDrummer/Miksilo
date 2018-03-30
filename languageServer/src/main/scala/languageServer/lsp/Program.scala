package languageServer.lsp

import ch.qos.logback.classic.Level
import com.typesafe.scalalogging.LazyLogging
import deltas.cloudformation.CloudFormationLanguage
import deltas.javac.JavaLanguage
import languageServer.MiksiloLanguageServer
import org.slf4j.LoggerFactory

import scala.util.Try

object Program extends LazyLogging {

  val languages = Map(
    "cloudFormation" -> CloudFormationLanguage.language,
    "java" -> JavaLanguage.getJava
  )

  def main(args: Array[String]): Unit = {
    LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[ch.qos.logback.classic.Logger].setLevel(Level.INFO)
    logger.debug(s"Starting server in ${System.getenv("PWD")}")

    val languageString = args.headOption
    val language = languageString.flatMap(s => languages.get(s)).getOrElse(languages.values.head)
    val connection = new JsonRpcConnection(System.in, System.out)
    val lspServer = Try {
      val languageServer = new MiksiloLanguageServer(language)
      new LSPServer(languageServer, connection)
    }
    lspServer.recover{case e => logger.error(e.getMessage); e.printStackTrace() }
    connection.listen()
  }
}

