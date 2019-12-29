package jsonRpc

object LazyLogging {
  var logger: Logger = VoidLogger
}
trait LazyLogging {
  def logger: Logger = LazyLogging.logger
}

class LambdaLogger(write: String => Unit) extends Logger {
  override def debug(message: String): Unit = {} //write(s"[DEBUG] $message\n")

  override def info(message: String): Unit = write(s"[INFO] $message\n")

  override def error(message: String): Unit = write(s"[ERROR] $message\n")
}

trait Logger {
  def debug(message: String): Unit
  def info(message: String): Unit
  def error(message: String): Unit
}

object VoidLogger extends Logger {
  override def debug(message: String): Unit = {}

  override def info(message: String): Unit = {}

  override def error(message: String): Unit = {}
}