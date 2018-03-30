package application

import java.awt.{AWTEvent, EventQueue}
import org.oxbow.swingbits.dialog.task.TaskDialogs

class ShowExceptionsInDialog extends EventQueue {
  override protected def dispatchEvent(newEvent: AWTEvent): Unit = {
    try
      super.dispatchEvent(newEvent)
    catch {
      case e: Exception =>
        TaskDialogs.showException(e)
    }
  }
}
