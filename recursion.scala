// Write a function that computes the elements of Pascal’s triangle
def pascal(c: Int, r: Int): Int =
  (c, r) match {
    case (0, _) => 1
    case (1, _) => 1
    case (a, b) if (a == b) => 1
    case (a, b) if (a > b) => throw new IllegalArgumentException("column can't be higher than row")
    case (a, b) pascal(a - 1, b - 1) + pascal(a, b - 1)
  }

// check balanced parentheses
def balance(chars: List[Char]): Boolean = {
  def balance_rec(remaining: List[Char], open_count: Int): Int =  
    if (remaining.isEmpty || open_count < 0) open_count
    else if (remaining.head == '(') balance_rec(remaining.tail, open_count + 1)
    else if (remaining.head == ')') balance_rec(remaining.tail, open_count - 1) 
    else balance_rec(remaining.tail, open_count)
  balance_rec(chars, 0) == 0
}

def map(lst: List[Int], f: Int=>Int): List[Int] =

