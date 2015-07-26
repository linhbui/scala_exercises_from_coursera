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
    (chars, count) match {
      case (_, c) if c < 0 => c
      case (Nil, c) => c
      case ('('::chs, c) => balanceRec(chs, c+1)
      case (')'::chs, c) => balanceRec(chs, c-1)
    }

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

def map[A, B](lst: List[A], f: A => B): List[B] = lst match {
  case Nil => Nil
  case a::as => f(a)::map(as, f)
}

def filter[A](lst: List[A], f: A => Boolean): List[A] = lst match {
  case Nil => Nil
  case a::as => if f(a) a::filter(as, f) else filter(as, f)
}

def zip[A, B](lst1: List[A], lst2: List[B]): List[(A, B)] = 
  (lst1, lst2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (a::as, b::bs) => (a, b)::zip(as, bs)
  }

def partition[A](lst: List[A], f: A => Boolean): (List[A], List[A]) = lst match {
  case Nil => (Nil, Nil)
  case a::as => partition(as) match {
    case (lst1, lst2) => if f(a) (a::lst1, lst2) else (lst1, a::lst2)
  }
}

def find[A](lst: List[A], f: A => Boolean): Option[A] = lst match {
  case Nil => None
  case a::as => if f(a) Some(a) else find(as)
}

def drop[A](lst: List[A], n: Int): List[A] = {
  if (n <= 0) lst
  else (lst, n) match {
    case (Nil, _) => Nil 
    case (a:as, n) => drop(as, n-1)
  }
}

def dropWhile[A](lst: List[A], f: A => Boolean): List[A] = lst match {
  case Nil => Nil
  case a::as => if f(a) dropWhile(as) else a::as
}

def foldLeft[A, B](lst: List[A], f: A => B => B, seed: B): B = lst match {
    case Nil => seed
    case a::as => foldLeft(as, f, f(a, seed))
}

def count[A](lst: List[A]): Int =
  foldLeft(lst, (_, b => b + 1) , 0)

def sum(lst: List[Int]): Int =
  foldLeft(lst, (_ + _) ,0)

def foldRight[A, B](lst: List[A], f: A => B => B, seed: B): B = lst match {
  case Nil => seed
  case a::as => f(a, foldRight(as, f, seed))
}

def flatten[A](lst: List[List[A]]): List[A] = lst match {
  case Nil => Nil
  case a::as => a ++ flatten(as)
}

def flatten[A](lst: List[List[A]]): List[A] =
  foldLeft(lst, (_ ++ _), Nil)

def flatMap[A, B](lst: List[A], f: A => List[B]): List[B] = lst match {
  case Nil => Nil
  case a::as => f(A) ++ flatten(map(as))
}
