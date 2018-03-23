package util

import scala.collection.mutable

class Statistics(parent: Statistics = null) {

  def profile[T](description: String, action: => T): T = {
    val start = System.nanoTime()
    val result = action
    val end = System.nanoTime()
    val timing = (end - start)/1000000.0
    if (parent != null)
      parent.add(description, timing)
    add(description, timing)
    result
  }

  val timingsPerKey: mutable.Map[Any, mutable.ArrayBuffer[Double]] = mutable.Map.empty

  def add(key: Any, timing: Double): Double = {
    val existing = timingsPerKey.getOrElseUpdate(key, mutable.ArrayBuffer.empty)
    existing += timing
    val average = existing.sum / existing.length
    average
  }

  def printAll(): Unit = {

    for(timingsForKey <- timingsPerKey.toSeq.sortBy(p => -1 * p._2.sum)) {
      val timings = timingsForKey._2
      val average = "%05.1f".format(timings.sum / timings.length)
      val total = timings.sum
      val totalString = "%06.0f".format(total)
      if (total > 10)
        System.out.println(s"${totalString}ms total, ${average}ms average, for ${timingsForKey._1}")
    }
  }
}
