package dp

import org.scalatest.{FlatSpec, Matchers}
//import dp.Fibonacci
class FibonacciSpec extends FlatSpec with Matchers {
//    solveRecursive(0) === 0L
//    solveRecursive(1) === 1L
  "fibonacci(5)" should "get be the same using different implementation" in {
    Fibonacci.solveWithRecursive(5) should be (5L)
    Fibonacci.solveWithLoop(5) should be (5L)
  }

}
