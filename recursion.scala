def pascal(c: Int, r: Int): Int =
  (c, r) match {
    case (0, _) => 1
    case (1, _) => 1
    case (a, b) if (a == b) => 1
    case (a, b) if (a > b) => throw new IllegalArgumentException("column can't be higher than row")
    case (a, b) pascal(a - 1, b - 1) + pascal(a, b - 1)
  }

