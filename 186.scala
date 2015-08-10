// Project Euler: Problem# 186
// Connectedness of a network

/*
The telephone number of the caller and the called number in record n are
Caller(n) = S2n-1 and Called(n) = S2n where S1,2,3,... come from the
"Lagged Fibonacci Generator":

For 1 ≤ k ≤ 55, Sk = [100003 - 200003k + 300007k3] (modulo 1000000)
For 56 ≤ k, Sk = [Sk-24 + Sk-55] (modulo 1000000)

If Caller(n) = Called(n) then the user is assumed to have misdialled and
the call fails; otherwise the call is successful.

From the start of the records, we say that any pair of users X and Y are
friends if X calls Y or vice-versa. Similarly, X is a friend of a friend
of Z if X is a friend of Y and Y is a friend of Z; and so on for longer
chains.

The Prime Minister's phone number is 524287. After how many successful
calls, not counting misdials, will 99% of the users (including the PM)
be a friend, or a friend of a friend etc., of the Prime Minister?
*/

import scala.collection.mutable.HashMap

object LaggedFibonacciGenerator {
  private val cache = new HashMap[Int, Int]()

  def getValue(n: Int): Int = {
    if (cache.contains(n)) {
      cache(n)
    }
    else {
      val laggedFib = calculateLaggedFib(n)
      cache += (n -> laggedFib)
      laggedFib
    }
  }

  private def calculateLaggedFib(n: Int): Int = {
    if (1 <= n && n <= 55) {
      (100003 - 200003*n + 300007*n*n*n) % 1000000
    }
    else {
      (getValue(n - 24) + getValue(n - 55)) % 1000000
    }
  }
}

class User(n: Int) {
  val number: Int = n
}
class Record(n: Int) {
  val caller = new User(LaggedFibonacciGenerator.getValue(2*n))
  val called = new User(LaggedFibonacciGenerator.getValue(2*n-1))
}
