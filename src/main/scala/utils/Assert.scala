package utils

class Assert[T](one: T) {
  def eq(a: String, b: String) = {
    if (a != b) {
      val assumption = false
      assert(assumption, s"$a != $b")
    }
  }

  def toString[X](x: X) = x match {
    case array: Array[_] => array.deep.toString()
    case other => other.toString
  }

  def ===[S](another: S): S = {
    val o = toString(one)
    val a = toString(another)
    if (o != a) {
      val assumption = false
      assert(assumption, s"$o != $a")
    }
    another
  }
  def !==[S](another: S): S = {
    val o = toString(one)
    val a = toString(another)
    if (o != a) {
      val assumption = false
      assert(assumption, s"$o != $a")
    }
    another
  }
}

object Assert {
  implicit def toAssert[T](one: T): Assert[T] = {
    new Assert(one)
  }
}