package cLanguage

import scala.collection.mutable

class Memory {
  case class Entry(size: Int, value: Any)
  val data = mutable.Map[Int,Entry]()
  val stackSize = 1000 * 1000

  def apply(location: Int) = data(location).value

  def alter(location: Int, value: Any) {
    data.put(location, new Entry(data(location).size, value))
  }

  def remove(location: Int) { data.remove(location) }

  def put(location: Int, value: Any, _type: Type) {
    val size = _type.size
    for(i <- location.until(location + size))
      data.remove(i)
    data.put(location,new Entry(size, value))
  }

  def putAlloc(value: Any, _type: Type) : Int = {
    val location = heapAlloc(_type.size)
    put(location,value,_type)
    location
  }

  def stackAlloc(size: Int) : Int = alloc(0, size)

  def heapAlloc(size: Int) = alloc(stackSize, size)

  private def alloc(location: Int, size: Int) : Int = {
    def allocFromLocation(location: Int) : Int = {
      if (!data.contains(location))
      {
        data.put(location, new Entry(size, 0))
        return location
      }

      allocFromLocation(location+data(location).size)
    }
    allocFromLocation(location)
  }

}
