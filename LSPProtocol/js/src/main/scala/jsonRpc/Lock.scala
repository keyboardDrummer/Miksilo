package cloudformation

import scala.concurrent.{Future, Promise}

class Lock {

  var promise: Promise[Unit] = null

  def notify2(): Unit = {
    if (promise != null) {
      promise.success(())
      promise = null
    }
  }

  def wait2(): Future[Unit] = {
    if (this.promise != null)
      throw new Error("woops")

    val promise = Promise[Unit]
    this.promise = promise
    promise.future
  }
}
