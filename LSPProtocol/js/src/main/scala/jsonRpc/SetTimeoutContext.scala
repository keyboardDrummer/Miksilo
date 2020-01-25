package jsonRpc

import scala.concurrent.ExecutionContext
import scala.scalajs.js.Dynamic.{global => g}

object SetTimeoutContext extends ExecutionContext {
  override def execute(runnable: Runnable): Unit = g.setTimeout(() => runnable.run(), 1)

  override def reportFailure(cause: Throwable): Unit = { ConsoleLogger.error(cause.getMessage) }
}
