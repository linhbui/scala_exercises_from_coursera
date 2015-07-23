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
  def balanceRec(chars: List[Char], open_count: Int): Int =  
    if (chars.isEmpty || open_count < 0) open_count
    else if (chars.head == '(') balanceRec(chars.tail, open_count + 1)
    else if (chars.head == ')') balanceRec(chars.tail, open_count - 1) 
    else balanceRec(chars.tail, open_count)

  balanceRec(chars, 0) == 0
}

// counts how many different ways you can make change for an amount, given a list of coin denominations
def countChange(money: Int, coins: List[Int]): Int = {
  def countChangeRec(money: Int, coins: List[Int]): Int =
    if (money == 0 || coins.isEmpty) 0
    else if (money >= coins.head) countChangeRec(money - coins.head, coins) + countChangeRec(coins.head, coins)
    else countChangeRec(money, coins.tail)

  countChangeRec(money, coins)
}

def map(lst: List[Int], f: Int=>Int): List[Int] =

