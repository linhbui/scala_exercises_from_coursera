def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = (a, b) match {
  case a > b zero
  case a <= b combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
}
