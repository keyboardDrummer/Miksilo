package cLanguage

import scala.collection.mutable

class Memory {
  case class Entry(size: Int, value: Any)
  val data = mutable.Map[Int,Entry]()
  val stackSize = 1000 * 1000

  def apply(location: Int) = data(location).value

  def put(location: Int, value: Any, _type: Type) {
    val size = _type.size
    for(i <- location.until(location + size))
      data.remove(i)
    data.put(location,new Entry(size, value))
  }

  def putAlloc(value: Any, _type: Type) : Int = {
    val location = alloc(_type.size)
    put(location,value,_type)
    location
  }

  def alloc(size: Int) : Int = {
    def allocFromLocation(location: Int) : Int = {
      if (!data.contains(location))
        return location

      allocFromLocation(location+data(location).size+1)
    }
    allocFromLocation(0)
  }
}
