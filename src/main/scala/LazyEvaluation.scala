object LazyEvaluation extends App {

  // lazy values are evaluated once, but only when they are used for the first time
  // lazy DELAYS the evaluation of values
  lazy val x: Int = throw new RuntimeException

  // this forces the evaluation of the expression above
  // once value is evaluated, it will stay assigned to that same name
  println(x)

  // the first time you print(y), the string hello will print; but the second time, it will not
  // the evaluation (which includes the print) happens the first time an assignment happens; afterwards, the value is already assigned
  lazy val y = {
    println("hello")
    7
  }
  println(y)
  println(y)

  // examples of implications:
  def sideEffectCondition: Boolean = {
    println("boo")
    true
  }
  def simpleCondition: Boolean = false

  lazy val lazyCondition = sideEffectCondition
  // won't see println(boo); lazyConidition isn't evaluated because of &&
  println(if (simpleCondition && lazyCondition) "yes" else "no")

  // in conjunction with call by name
  def byNameMethod(n: => Int): Int = n + n + n + 1
  def retrieveMagicValue = {
    println("waiting")
    Thread.sleep(1000)
    42
  }

  // value n is evaluated 3 times with byName method above
  println(byNameMethod(retrieveMagicValue))

  //better way is to use lazy vals instead; technique is called `CALL BY NEED`
  // evaluate parameter only when you need it, and use the same value in the rest of the code
  def byNameMethodBetter(n: => Int): Int = {
    lazy val t = n
    t + t + t + 1
  }

  // filtering with lazy val
  def lessThan30(i: Int): Boolean = {
    println(s"$i is less than 30")
    i < 30
  }

  def greaterThan20(i: Int): Boolean = {
    println(s"$i is gt 20")
    i > 20
  }

  // withFilter: uses lazy values under the hood; creates a filter monadic
  val numbers = List(1,25,40,25)
  val lt30 = numbers.filter(lessThan30)
  val gt20 = lt30.filter(greaterThan20)

  val lt30Lazy = numbers.withFilter(lessThan30)
  val gt20Lazy = lt30Lazy.withFilter(gt20Lazy)
  println(gt20Lazy) // nothing actually prints aside from a cryptics string rep; the methods aren't actually being called in the lazy version
  gt20Lazy.foreach(println) // this forces the print; it checks the nth element with both filters than goes to n+1 element of original List

  // for-comprehensions use withFilter with guards; the two things below are equivalent
  for {
    a <- List(1,2,3) if a % 2 == 0 // use lazy vals
  } yield a + 1
  //
  List(1,2,3).withFilter(_ % 2 == 0).map(_ + 1)

  /* implement a lazily evaluated, singly linked STREAM of elements.
   - head of stream is always evaluated and available
   - tail of stream is always lazily evaluated, available only on demand
   */

  abstract class MyStream[+A] {
    def isEmpty: Boolean
    def head: A
    def tail: MyStream[A]

    // right associative; prepend element
    def #::[B >: A](element: B): MyStream[B]
    // concat
    def ++[B >: A](anotherStream: MyStream[B]): MyStream[B]

    def foreach(f: A => Unit): Unit
    def map[B](f: A => B): MyStream[B]
    def flatMap[B](f: A => MyStream[B]): MyStream[B]
    def filter[A](f: A => Boolean): MyStream[A]

    // take first n elements out of this stream
    def take(n: Int): MyStream[A]
    def takeAsList(n: Int): List[A]
  }

  object MyStream {

    // generate stream based on a start element and a generator function
    // will generate next value based on previous value known in the stream
    // naturals = MyStream.from(1)(x => x + 1) = stream of natural numbers : infinite stream
    // naturals.take(100) // lazily evaluated stream of first 100 naturals
    // naturals.take(100).foreach(println) // will work
    // naturals.foreach(println) // will crash
    def from[A](start: A)(generator: A => A): MyStream[A]
  }
}
