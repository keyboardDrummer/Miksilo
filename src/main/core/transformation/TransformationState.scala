package core.transformation

import scala.collection.mutable
import scala.util.Random

class TransformationState {
  val data: mutable.Map[Contract, Any] = mutable.Map.empty

  def getUniqueLabel(prefix: String) = prefix + getGUID

  def getGUID: Long = Random.nextLong()
}
