package objsets

import common._
import TweetReader._

class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 */
abstract class TweetSet {

  def filter(p: Tweet => Boolean): TweetSet =
    filterAcc(p, this)

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  def union(that: TweetSet): TweetSet

  def mostRetweeted: Tweet

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   */
  def descendingByRetweet: TweetList


  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = this

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()

  def union(that: TweetSet): TweetSet = that

  def mostRetweeted: Tweet = throw new java.util.NoSuchElementException("No Tweet on Empty TweetSet")

  def descendingByRetweet: TweetList = Nil
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet =
    if (p(elem)) new NonEmpty(elem, filterAcc(p, left), filterAcc(p, right))
    else (left, right) match {
      case _: (Empty, _) => filterAcc(p, right)
      case _: (_, Empty) => filterAcc(p, left)
      case _ => filterAcc(p, left) union filterAcc(p, right)
    }

  def mostRetweeted: Tweet =
    def findMax(a: Tweet, b: Tweet): Tweet =
      if (a.retweets > b.retweets) a else b

    (left, right) match {
      case _: (Empty, Empty) => elem
      case _: (Empty, _) => findMax(right.mostRetweeted, elem)
      case _: (_, Empty) => findMax(left.mostRetweeted, elem)
      case _ => findMax(elem, findMax(left.mostRetweeted, right.mostRetweeted))
    }

  def descendingByRetweet: TweetList = {
    val tweet = mostRetweeted
    new Cons(tweet, remove(tweet).descendingByRetweet)
  }

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def union(that: TweetSet): TweetSet = ((left union right) union that) incl elem

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}

object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet = {

    def stringContainedIn(txt: String)(s: String): Boolean = txt contains s

    def hasGoogleKeywords(t: Tweet): Boolean = google.exists(stringContainedIn(t.text))

    TweetReader.allTweets.filter(t => hasGoogleKeywords(t))
  }

  lazy val appleTweets: TweetSet = TweetReader.allTweets.filter(t => apple.exists(s => t.text.contains(s)))

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = (googleTweets union appleTweets).descendingByRetweet
}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}
