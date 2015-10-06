package dp

import utils.debug

import scala.collection.mutable
import utils.Assert._

object Fibonacci {
  // solve it using recursive
  // execute sequence =>
  // try to compute f(5), it is not in the table
  // f(5) = f(4) + f(3) NOTE: the order of invocation matters
  // try to compute f(4), it is not in the table
  // f(4) = f(3) + f(2)
  // try to compute f(3), it is not in the table
  // f(3) = f(2) + f(1)
  // try to compute f(2), it is not in the table
  // f(2) = f(1) + f(0)
  // f(1) & f(0) already in the table as initial value
  // store f(2) into the table
  // then it go back to f(3) = f(2) + f(1)
  // f(2) & f(1) already in the table
  // store f(3) into the table
  // then it go back to f(4) = f(3) + f(2)
  // f(3) & f(2) already in the table
  // store f(4) into the table
  // then it go back to f(5) = f(4) + f(3)
  // f(4) & f(3) already in the table
  // return value f(5)
  val tbl = mutable.Map(0 -> BigInt(0), 1 -> BigInt(1))
  def solveWithRecursive(n: Int): BigInt = {
    val opt = tbl.get(n)
    opt match {
      case None =>
        // the value has not been computed
        // then we try to compute f(n-1) and f(n-2)
        // then save it into the table
        debug(s"compute  f($n)")
        val value = solveWithRecursive(n - 1) + solveWithRecursive(n - 2)
        debug(s"computed f($n) = $value")
        tbl(n) = value
        value
      case Some(value) =>
        debug(s"get      f($n) = $value")

        value
    }
  }


  def solveV2[V](v0: V, v1: V,
               fun: (V, V) => V,
               n: Int): V = {
    var a = v0
    var b = v1
    for (i <- 2 to n) {
      val tmp = fun(a, b)
      a = b
      b = tmp
    }
    b
  }


  def solve[V](initValues: Map[Int, V],
               fun: (V, V) => V,
               n: Int): V = {
    val table = mutable.Map[Int, V](initValues.toSeq: _*)
    for (i <- 2 to n) {
      table(i) = fun(table(i - 1), table(i - 2))
    }
    table(n)
  }

  def solveUseGeneric(n: Int): BigInt = {
    val init = Map(0 -> BigInt(0), 1 -> BigInt(1))
    val roundConst = (0 until 8).fold(1)((result, item) => result * 10) + 7
    val fun = (v1: BigInt, v2: BigInt) => (v1 + v2) % roundConst

    solve(init, fun, n)
  }

  def solveUseV2(n: Int): BigInt = {
    val init = Map(0 -> BigInt(0), 1 -> BigInt(1))
    val roundConst = (0 until 8).fold(1)((result, item) => result * 10) + 7
    val fun = (v1: BigInt, v2: BigInt) => (v1 + v2) % roundConst

    solveV2(BigInt(0), BigInt(1), fun, n)
  }
  // this is a bottom up algorithm
  def solveWithLoop(n: Int): BigInt = {
    val table = mutable.Map(0 -> BigInt(0), 1 -> BigInt(1))
    for (i <- 2 to n) {
      table(i) = table(i - 1) + table(i - 2)
    }
    table(n)
  }



  def main(args: Array[String]): Unit = {
    //    solveRecursive(0) === 0L
    //    solveRecursive(1) === 1L
    solveWithRecursive(5) === 5L
    //    solveRecursive(10) === 55L
    //    solveRecursive(100) == 24278230L
    println(solveWithLoop(5000))
    println(solveUseGeneric(5000))
    println(solveUseV2(5000))
  }
}

