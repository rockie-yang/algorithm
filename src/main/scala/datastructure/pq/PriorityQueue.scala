package datastructure.pq

/**
 * Created by : Rockie Yang(eyouyan@gmail.com, snowriver.org)
 * Created at : 10/3/15
 */
class PriorityQueue[T](sz: Int)(implicit val ord: Ordering[T]) {
  import ord._
  val max_size = sz + 1 // item 0 is not used
  var tail_pos = 0
  val array = new Array[AnyRef](max_size)

  def size = tail_pos
  def debug(): Unit = {
    val s = array.filter(_ != null).mkString(" ")
    println(s)
  }
  def enqueue(v: T): Unit = {
    tail_pos += 1
    array(tail_pos) = v.asInstanceOf[AnyRef]
    swim(tail_pos)
    debug()
  }
  
  def max: Option[T] = {
     if (tail_pos > 0) {
       val m = array(1).asInstanceOf[T]
       array(1) = array(tail_pos)
       array(tail_pos) = null
       tail_pos -= 1
//       array(tail_pos) = 0
       sink(1)
       debug()
       Some(m)
     }
    else None
  }

  def sink(pos: Int): Unit = {
    var maxIndex = pos
    val left = pos << 2
    if (left <= tail_pos && array(maxIndex).asInstanceOf[T] < array(left).asInstanceOf[T]) {
      maxIndex = left
    }
    val right = left + 1
    if (right <= tail_pos && array(maxIndex).asInstanceOf[T] < array(right).asInstanceOf[T]) {
      maxIndex = right
    }
    if (pos != maxIndex) {
      val tmp = array(pos)
      array(pos) = array(maxIndex)
      array(maxIndex) = tmp
      sink(maxIndex)
    }
  }
  
  def swim(pos: Int): Unit = {
    if (pos > 1) {
      val parentIndex = pos / 2
      if (array(pos).asInstanceOf[T] > array(parentIndex).asInstanceOf[T]) {
        val tmp = array(pos)
        array(pos) = array(parentIndex)
        array(parentIndex) = tmp
        swim(parentIndex)
      }
    }
  }
}

object PriorityQueue extends App{
  val q = new PriorityQueue[Char](5)
  q.enqueue('A')
  q.enqueue('T')
  q.enqueue('C')
  println(q.max)
  println(q.max)
  println(q.max)
  println(q.size)
}
