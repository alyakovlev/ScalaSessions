/*
  The sieve of Eratosthenes,
  one of a number of prime number sieves,
  is a simple, ancient algorithm for finding all prime numbers up to any given limit.

  https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
*/
def from(n: Int): Stream[Int] = n #:: from(n+1)

def sieve(s: Stream[Int]): Stream[Int] = {
  s.head #:: sieve(s.tail filter ((i: Int) => i % s.head != 0))
}

val primes = sieve(from(2))

(primes take 30).toList
