package datastructure.mergesort

object MergeSort {
  def apply[T](data: Array[T])(implicit ord: Ordering[T]): Unit = {
    sort(data)(ord)
  }

  implicit def any2T[T](v: AnyRef): T = v.asInstanceOf[T]

  def sort[T](data: Array[T], bottomsUp: Boolean = false)(implicit ord: Ordering[T]): Unit = {
    val aux = new Array[AnyRef](data.length)
    var numMerge = 0
    def merge(lo: Int, mid: Int, hi: Int): Unit = {
      numMerge += 1
      for (i <- lo to hi) {
        aux(i) = data(i).asInstanceOf[AnyRef]
      }
      import ord._
      var i = lo
      var j = mid + 1
      for (k <- lo to hi) {
        if (i > mid) {
          data(k) = aux(j)
          j += 1
        }
        else if (j > hi) {
          data(k) = aux(i)
          i += 1
        }
        else if (any2T[T](aux(i)) < any2T[T](aux(j))) {
          data(k) = aux(i)
          i += 1
        }
        else {
          data(k) = aux(j)
          j += 1
        }
      }
      if (numMerge == 7) {
        println(data.mkString(" "))
      }
    }
    def sort0(lo: Int, hi: Int): Unit = {
      if (hi > lo) {
        val mid = lo + (hi - lo) / 2
        sort0(lo, mid)
        sort0(mid + 1, hi)
        merge(lo, mid, hi)
      }
    }
    def sortb(length: Int): Unit = {
      var sz = 1
      while (sz < length) {
        val sz2 = sz << 1
        for (lo <- 0 until length by sz2) {
          merge(lo, lo + sz - 1, math.min(lo + sz2 - 1, length - 1))
        }
        sz = sz2
      }
    }

    if (bottomsUp)
      sortb(data.length)
    else
      sort0(0, data.length - 1)
  }

  def main(args: Array[String]): Unit = {
    val data = Array(81, 67, 21, 25, 36, 13, 33, 20, 71, 88, 82, 66)
    sort(data)
    println(data.mkString(" "))
    val data2 = Array(91, 69, 85, 52, 56, 10, 66, 19, 39, 23)
    sort(data2, bottomsUp = true)
  }
}
