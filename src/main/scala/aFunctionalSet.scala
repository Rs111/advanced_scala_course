object aFunctionalSet extends App {

  // sets are un-ordered collections with no duplicates
  val set = Set(1,2,3)

  // sets are `callable`; they have apply which returns boolean if element is in set
  // they behave very much like functiond
  set(2) //true
  set(42) // false

  // sets actually implemented as functions: trait Set[A] extends (A) => Boolean with ...
}

trait MySetUnimplemented[A] extends (A => Boolean) {

  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]
  def -(elem: A): MySet[A]
  def --(anotherSet: MySet[A]): MySet[A]
  def &(anotherSet: MySet[A]): MySet[A]

  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit

  override def apply(x: A): Boolean
}


trait MySet[A] extends (A => Boolean) {

  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]
  def -(elem: A): MySet[A]
  def --(anotherSet: MySet[A]): MySet[A]
  def &(anotherSet: MySet[A]): MySet[A]

  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit

  def unary_! : MySet[A]

  def apply(elem: A): Boolean = contains(elem) // this is general contract of MySet
}

// using class not object because MySet[A] is invariant (can make it covariant later)
class EmptySet[A] extends MySet[A] {

  def contains(elem: A): Boolean = false
  def +(elem: A): MySet[A] = new NonEmptySet[A](head=elem, tail=this)
  def ++(anotherSet: MySet[A]): MySet[A] = anotherSet
  def -(elem: A): MySet[A] = this
  def --(anotherSet: MySet[A]): MySet[A] = this
  def &(anotherSet: MySet[A]): MySet[A] = this

  def map[B](f: A => B): MySet[B] = new EmptySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]
  def filter(predicate: A => Boolean): MySet[A] = this
  def foreach(f: A => Unit): Unit = () // () is unit value

  // negating an empty set gives you all inclusive set
  // need to create AllInclusiveSet just for this
  // AllInclusiveSet implemented as a property-based set where the property is always true
  def unary_! : MySet[A] = new PropertyBasedSet[A](_ => true)
}

// denotes all elements of type A that satisfy the property
// in math terms: { x in A | property(x) }
class PropertyBasedSet[A](property: A => Boolean) extends MySet[A] {

  def contains(elem: A): Boolean = property(elem) // when is an element contained in a property based set? it is contained if property holds
  /*
    when do we add an element to a property based set? in other words, what kind of property do we obtain after adding an element to a property based set?
    { x in A | property(x) } + element = {x in A | property(x) || x == element}
   */
  def +(elem: A): MySet[A] = new PropertyBasedSet[A](x => property(x) || x == elem)

  // same logic as above: { x in A | property(x) } ++ set => { x in A | property(x) || set contains x }
  def ++(anotherSet: MySet[A]): MySet[A] = new PropertyBasedSet[A](x => property(x) || anotherSet.contains(x))

  // by removing an element, we are saying: the x meets the property and x cannot be the element we removed
  def -(elem: A): MySet[A] = filter(x => x != elem) //new PropertyBasedSet[A](x => property(x) && x != elem)
  def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet) //new PropertyBasedSet[A](x => property(x) && !anotherSet.contains(x))
  def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet) // anotherSet is callable; this.filter(x => anotherSet.contains(x))...or just do anotherSet(x) for apply method
  // another possible implementation (by me):  new PropertyBasedSet[A](x => property(x) && anotherSet.contains(x))

  // can't really map and flatMap a property based set because you don't know what you're going to obtain
  // e.g. all integers => (_ % 3) => [0,1,2]; problem is that once we get into that, we won't know that is is actually a finite set
  // if you map an infinite set with a function you will not know whether the resulting set is finite or not, so you won't be able to tell if an element is in the set or not
  // this breaks the whole point of the set
  def map[B](f: A => B): MySet[B] = politelyFail
  def flatMap[B](f: A => MySet[B]): MySet[B] = politelyFail
  // only hold elements for which both property and predicate hold up
  def filter(predicate: A => Boolean): MySet[A] = new PropertyBasedSet[A](x => property(x) && predicate(x))
  def foreach(f: A => Unit): Unit = politelyFail
  // e.g. if your property is even-ness, your unary_bang will return all numbers for which even-ness doesn't hold; i.e. all odd
  def unary_! : MySet[A] = new PropertyBasedSet[A](x => !property(x))

  def politelyFail = throw new IllegalArgumentException("really deep rabbit hole")
}

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {

  def contains(elem: A): Boolean = {
    elem == head || tail.contains(elem)
  }

  def +(elem: A): MySet[A] = {
    if (this contains elem) this
    else new NonEmptySet[A](head = elem, tail = this)
  }

  // this is wild but somehow works
  /*
  [1,2,3] ++ [4,5]
  = [2,3] ++ [4,5] + 1
  = [3]   ++ [4,5] + 1 + 2
  = []    ++ [4,5] + 3 + 2 + 1 // the ++ is defined for an EmptySet
   */
  def ++(anotherSet: MySet[A]): MySet[A] = {
    this.tail ++ anotherSet + this.head
  }

  def -(elem: A): MySet[A] =
    if (head == elem) tail // no duplicates in a set; this wouldn't work for other collections
    else new NonEmptySet[A](head, tail - elem) // else tail - elem + head
  def --(anotherSet: MySet[A]): MySet[A] = filter(x => !anotherSet(x)) // `this.filter(x => !anotherSet.contains(x))` is long way
  def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet) // `this.filter(x => anotherSet.contains(x))` is long way

  // negation of a set; to build this, we need a `PropertyBasedSet`
  // set[1,2,3] =>
  def unary_! : MySet[A] = new PropertyBasedSet[A](x => !this.contains(x))

  def map[B](f: A => B): MySet[B] = (tail map f) + f(head)
  def flatMap[B](f: A => MySet[B]): MySet[B] = (tail flatMap f) ++ f(head)
  def filter(predicate: A => Boolean): MySet[A] = {
    val filteredTail = tail filter predicate
    if (predicate(head)) filteredTail + head
    else filteredTail
  }
  def foreach(f: A => Unit): Unit = {
    f(head)
    tail foreach f
  }
}

object MySet {
  /*
  val s = MySet(1,2,3) this calls buildSet(seq(1,2,3), [])
  = buildSet(seq(2,3), [] + 1)
  = buildSet(seq(3), [1] + 2)
  = buildSet(seq(), [1,2] + 3) // what is returned
  */
  def apply[A](values: A*): MySet[A] = {
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] = {
      if (valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head)
    }

    buildSet(values.toSeq, new EmptySet[A])
  }
}


object MySetPlayground extends App {
  val s = MySet(1,2,3,4)
  s + 5 ++ MySet(-1, -2) + 3 flatMap(x => MySet(x, 10 * x)) filter (_ % 2 == 0) foreach println

  val negative = !s // s.unary_! = all the naturals not equal to 1,2,3,4
  println(negative(2))
  println(negative(5))

  val negativeEven = negative.filter(_ % 2 == 0)
  println(negativeEven(5))
  val negativeEven5 = negativeEven + 5 // all the even numbers greater than 4 plus 5
  println(negativeEven5(5))
}





/*****Sequences and Maps ******/
/*
 Notes
 - Sequences are callable through an integer index
 - if you call a sequence, it will either give you the element at index or throw an outOfBound exception
 trait Seq[+A] extends PartialFunction[Int, A] {
    def apply(index: Int): A
 }

 val numbers = List(1,2,3)
 numbers(5)

 // map defined on domain of its keys, but doesn't take full domain
 trait Map[A, +B] extends PartialFunction[A,B] {
    def apply(key: A): B
    def get(key: A): Option[B]
 }

 eg:
 val phoneMappings = Map(2 -> "ABC", 3 -> "DEF")
 phoneMappings(2)
 phoneMappings(1) NoSuchElementException
 */

