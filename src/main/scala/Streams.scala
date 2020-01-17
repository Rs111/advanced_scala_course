import scala.annotation.tailrec

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
  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B]

  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): MyStream[B]
  def flatMap[B](f: A => MyStream[B]): MyStream[B]
  def filter[A](predicate: A => Boolean): MyStream[A]

  // take first n elements out of this stream
  def take(n: Int): MyStream[A]

  @tailrec
  final def toList[B >: A](acc: List[B] = Nil): List[B] =
    if (isEmpty) acc
    else tail.toList(head :: acc)
}

object EmptyStream extends MyStream[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException
  def tail: MyStream[Nothing] = throw new NoSuchElementException

  // right associative; prepend element
  def #::[B >: Nothing](element: B): MyStream[B] = new Cons(element, this)
  // concat
  def ++[B >: Nothing](anotherStream: => MyStream[B]): MyStream[B] = anotherStream

  def foreach(f: Nothing => Unit): Unit = ()
  def map[B](f: Nothing => B): MyStream[B] = this
  def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this
  def filter[A](predicate: A => Boolean): MyStream[A] = this

  // take first n elements out of this stream
  def take(n: Int): MyStream[Nothing] = this
}

// tl is byName; helps in lazily evaluating the stream
class Cons[+A](hd: A, tl: => MyStream[A]) extends MyStream[A] {
  def isEmpty: Boolean = false

  // notice that head is val while tail is lazy val; combining `call by name` parameter with lazy val is `call by need`
  // call by need is technique we use in Cons implementation
  override val head: A = hd // overriding as a val; overriding because it might be needed more than once throughout implementation
  lazy val tail: MyStream[A] = tl

  // right associative; prepend element
  /*
    - how does this operator work while still preserving the lazy evaluation?
    val s = new Cons(1, EmptyStream) // empty stream is lazily evaluated; will not be evaluated inside Cons until it's needed
    val prepended = 1 #:: s = new Cons(1, s) // s will still be lazily evaluated after the prepend; whatever is at the tail of s will remain unevaluated
   */
  def #::[B >: A](element: B): MyStream[B] = new Cons(element, this)
  // concat two streams
  /*
    tail ++ anotherStream is lazily evaluated, which means evaluated when needed
    - the tail in  the concatenation will itself only be evaluated when needed
    - thereofre ++ still preserves lazy evaluation in the stream
    - unsure how it works
    - new Cons(head, tail ++ anotherStream) -> new Cons(head, this.tail
    - //[1,2] ++ [3,4,5] => new Cons(1, new Cons(2, Empty ++ [3,4,5])) => new Cons(1, new Cons(2, new Cons(3, new Cons(4, new Cons(5, Empty))
    - very important to call by name to delay the evaluation of the right side; e.g. delay evaluation of tail.flatMap(f)
   */
  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] = new Cons(head, tail ++ anotherStream)

  // will force evaluation
  def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }
  def map[B](f: A => B): MyStream[B] = new Cons(f(head), tail.map(f))
  def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f) // new Cons(f(head).head, f(head).tail ++ tail.flatMap(f))
  def filter[A](predicate: A => Boolean): MyStream[A] =
    if (predicate(head)) new Cons(head, tail.filter(predicate))
    else new Cons(tail.head, tail.tail.filter(predicate)) //tail.filter(predicate)  // new Cons(tail.head, tail.tail.filter(predicate))

  // take first n elements out of this stream
  def take(n: Int): MyStream[A] =
    if (n <= 0) EmptyStream
    else if (n == 1) new Cons(head, EmptyStream)
    else new Cons(head, tail.take(n-1))


}

object MyStream {

  // generate stream based on a start element and a generator function
  // will generate next value based on previous value known in the stream
  // naturals = MyStream.from(1)(x => x + 1) = stream of natural numbers : infinite stream
  // naturals.take(100) // lazily evaluated stream of first 100 naturals
  // naturals.take(100).foreach(println) // will work
  // naturals.foreach(println) // will crash
  def from[A](start: A)(generator: A => A): MyStream[A] = {
    new Cons(start, from(generator(start))(generator))
  }
}


object StreamPlayground extends App {

  val naturals = MyStream.from(1)(_ + 1)
  println(naturals.head) // 1
  println(naturals.tail.head) // 2
  println(naturals.tail.tail.head) // 3

  val startFrom0 = 0 #:: naturals // naturals.#::(0)
  println(startFrom0.head)

  startFrom0.take(10000).foreach(println)

  // map, flatMap
  println(startFrom0.map(_ * 2).take(100).toList())
  println(startFrom0.flatMap(x => new Cons(x, new Cons(x + 1, EmptyStream))).take(10).toList())
  println(startFrom0.take(10).filter(x => x < 10).take(10).toList()) // filter is wrong

  /* 1 - stream of fibonacci
  [first, [....]]  tail should be a recursive call of fibonacci
  [first, fibo(second, first + second)]
   */
  def fibonacci(first: Int, second: Int): MyStream[Int] =
    new Cons(first, fibonacci(second, first + second))

  println(fibonacci(1, 1).take(20).toList())
  /* 2 - stream of Eratosthenes sieve

  [2,3,4,5,6,7,8,9,10,11,12 ...
  [2 3 4 7 9 11 13
  [2 eratosthenes applied to (numbers filtered by n % 2 != 0)
  [2 3 eratosphenes applied to [5,7,9 11, ...] filtered by n % 3 != 0
  */
  def eratosthenes(numbers: MyStream[Int]): MyStream[Int] =
    if(numbers.isEmpty) numbers
    else new Cons(numbers.head, eratosthenes(numbers.tail.filter(n => n % numbers.head != 0)))
}

