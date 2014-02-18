package transformation

import scala.collection.mutable
import scala.util.Random

class TransformationState {
  val data: mutable.Map[ProgramTransformation,Any] = mutable.Map.empty
  def getGUID : Long = Random.nextLong()
  def getUniqueLabel(prefix: String) = prefix + getGUID
}
