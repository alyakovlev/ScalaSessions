/*
  Решето Эратосфена - алгоритм нахождения целых чисел
  https://ru.wikipedia.org/wiki/Решето_Эратосфена
*/
def from(n: Int): Stream[Int] = n #:: from(n+1)

def sieve(s: Stream[Int]): Stream[Int] = {
  s.head #:: sieve(s.tail filter ((i: Int) => i % s.head != 0))
}

val primes = sieve(from(2))

(primes take 30).toList
