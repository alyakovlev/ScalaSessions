/*
*Instructions and Descriptions here:
*https://class.coursera.org/progfun-004/assignment/view?assignment_id=3
*/
object FunSets {

  type Set = Int => Boolean

  def contains(s: Set, elem: Int): Boolean = s(elem)

  def singletonSet(elem: Int): Set = (n : Int) => n == elem

  def union(s: Set, t: Set): Set = (i : Int) => s(i) || t(i)

  def intersect(s: Set, t: Set): Set = (i : Int) => s(i) && t(i)

  def diff(s: Set, t: Set): Set = (i : Int) => s(i) && !t(i)

  val bound = 1000

  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a>bound) true
      else if (diff(s,p)(a)) false
      else iter(a+1)
    }
    iter(-bound)
  }

  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, (x : Int) => !p(x))

  def map(s: Set, f: Int => Int): Set = (y => exists(s, x => y == f(x)))

  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  def printSet(s: Set) {
    println(toString(s))
  }
}

def positivNumbers = (n : Int) => n > 0
FunSets.contains(positivNumbers, 2)
FunSets.toString(FunSets.singletonSet(4))

def firstSet = (n : Int) => (n > 0 && n <= 15)
def secondSet = (n : Int) => (n > 10 && n <= 20)
FunSets.toString(FunSets.union(firstSet, secondSet))

FunSets.toString(FunSets.intersect(firstSet, secondSet))

FunSets.toString(FunSets.diff(firstSet, secondSet))

FunSets.forall(firstSet, secondSet)

FunSets.exists(firstSet, secondSet)

FunSets.toString(FunSets.map(firstSet, x => x * x))