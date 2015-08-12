def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = (a, b) match {
  case a > b zero
  case a <= b combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
}

object FunSets {
  type Set = Int => Boolean

  def contains(s: Set, elem: Int): Boolean = s(elem)

  def singletonSet(elem: Int): Set = (elem == _)

  def union(s: Set, t: Set): Set = x => (contains(s, x) || contains(t, x))

  def intersect(s: Set, t: Set): Set = x => (contains(s, x) && contains(t, x))

  def diff(s: Set, t: Set): Set = x => (contains(s, c) && !contains(t, x))

  def filter(s: Set, p: Int => Boolean): Set = x => (contains(s, x) && p(x))

  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > 1000) true
      else if (contains(s, a)) p(a) && iter(a+1)
      else iter(a + 1)
    }
    
    iter(-1000)
  }
}
