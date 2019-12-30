import scala.util.Random

object PatternMatching extends App {

  // pattern matching is like a switch on steroids
  val random = new Random
  val x = random.nextInt(10)

  // expression x match {} is called pattern matching; tries to match a value against multiple patterns; each pattern is written in a case statement
  // case 'pattern' => result
  val description = x match {
    case 1 => "the ONE"
    case 2 => "double or nothing"
    case 3 => "third time is the charm"
    case _ => "something else" // _ = WILDCARD

  }

  println(x)
  println(description)


  // pattern matching properly
  // 1. pattern matching can Decompose values
  //EG declare case class called Person
  case class Person(name: String, age: Int)
  val bob = Person("bob", 20)

  //if bob is a Person, pattern matching is able to deconstruct bob into constituent parts (even though pattern match doesn't know them beforehand)
  val greeting = bob match {
    case Person(n, a) if a < 21 => s"Hi my name is $n and I am less than 21 years old" //can also have guards with pattern matching
    case Person(n, a) => s"Hi my name is $n and I am $a years old"
    case _ => "I don't know who I am"
  }

  println(greeting)

  /* Notes so far
  1. cases are matched in order; first matched pattern is returned
  2. what is no case is matched? scala.MatchError (cover your ass with with wildcards)
  3. type of a match expression is the unification of all possible return types; e.g. for 'bob match {}', the type of greeting is a string
  4. PM works very well with case classes; they come with extractor classes out of the box
  5. don't try to match everything where you don't need to match
  */

  // 2. pattern matcging on sealed hierarchies
  sealed class Animal
  case class Dog(breed: String) extends Animal
  case class Parrot(greeting: String) extends Animal

  val animal: Animal = Dog("Terra Nova")
  animal match {
    case Dog(someBreed) => println(s"matched dog of breed $someBreed") //you get a warning that this may not be exhaustive, if Animal class is sealed (if not sealed, no warning)...this helps you cover your ass
  }


  /*Exercises
  1. write simple function that uses pattern matching; takes an Expr as a param and returns human readable form of it

  EG Sum(number2, number3) => 2 + 3 (string)
  Sum( Number(2), sum(Number(3), Number(4)) => 2 + 3 + 4
  Product(sum(1,2), 3) => (1 + 2) * 3
   */

  trait Expr
  case class Number(n: Int) extends Expr
  case class Sum(e1: Expr, e2: Expr) extends Expr
  case class Prod(e1: Expr, e2: Expr) extends Expr

  def show(e: Expr): String = e match {
    case Number(n) => s"$n"
    case Sum(e1, e2) => show(e1) + " + " + show(e2)
    case Prod(e1, e2) => {

      def maybeShowParentheses(exp: Expr) = exp match {
        case Prod(_, _) => show(exp)
        case Number(_) => show(exp)
        case _ => "(" +  show(exp) + ")"
      }

      maybeShowParentheses(e1) + " * " + maybeShowParentheses(e2)
    }
  }

  println(show(Sum(Number(2), Number(3))))



  // ALL THE PATTERNS
  // 1. match on constants
  val xNew: Any = "scala"

  val constant = xNew match {
    case 1 => "Number"
    case "scala" => "THE scala"
    case true => "Boolean"
    case PatternMatching=> "object"
  }

  // 2. match anything
  // 2.1 wildcard (an implementation of the above)

  val anything1 = xNew match {
    case _ => "anything!"
  }

  // 2.1 variable (another implementation)
  // property of using the variable 'something' is that it i) matches anything and ii) can be used in return expression
  val anything2 = xNew match {
    case something => s"it is $something" // this returns 'it is scala'; this basically assigns xNew to something
  }
  println(anything2)


  // 3 - tuples
  val aTuple = (1,2)
  val matchATuple = aTuple match {
    case (1, 1) => "found it"
    case (something, 2) => s"$something"
  }

  //can PM (pattern match) nested tuples
  val nestedTuple = (1, (1,2))
  val matchNestedTuple = nestedTuple match {
    case (_, (1, v)) => s"$v is the second element of nested tuple"
  }


  // 4- case classes - constructor pattern (seen above)

  // 5 - lists
  val aList = List(1,2,3)
  val matchAList = aList match {
    case Nil => "empty"
    case List(1, v, 2, _, _) => s"has 1, second is $v"
    case List(1, _*) => "list starts with 1" //varargs, list of arbitrary length
    case 1 :: List(_) =>    // infix pattern
    case List(1,2,3) :+ 42 =>   //another infix pattern
  }


  // 6 - type specifier; force pattern matching to conform to certain types
  val unknown: Any = 2
  val unknownMatch = unknown match {

    case list: List[Int] => "list of int"
    case _ => "not list of int"
  }


  // 7 - name binding: name a pattern; e.g. first pattern is named 'nonEmptyList
  val anotherList = List(1,2,3,4,5)
  val matchAnotherList = anotherList match {
    case nonEmptyList @ List(1,2,3,4,5) => s"$nonEmptyList is equal to 1,2,3,4,5"
    case List(1, second @ 2) => "can name the variables inside a list too" //name-binding inside nested patterns
    case _ => "blah"
  }


  // 8 - multi patterns (note: multi-patterns are multiple patterns chained by a pipe operator)
  val a = 2
  val aMatched = a match {

    case 2 | 3 => "a is 2 or 3"
    case _ => "not 2 or 3"
  }


  // 9 - if guards
  val aSmallList = List(1,2)
  val matchSecondElement = aSmallList match {

    case List(_, secondElement) if secondElement % 2 == 0 => "second element is divisible by 2"
    case _ => "not divisible by 2"
  }


  //SURPRISE; Java designed for backward compatability leads to this; generic types only introduced in Java 5
  // in java 1, List[Int] or List[String] is just List; therefore the first thing is a match
  // this shit is called 'type erasure'
  val aListOfNumbers = List(1,2,3)

  val matchListAbove = aListOfNumbers match {

    case stringsList: List[String] => "list of strings"
    case numbersList: List[Int] => "list of int"
    case list: List[Any] => "any list" //basically reads all List[type] as this
    case _ => ""
  }



  /******Big Ideas of pattern matching*****/
  // Big Idea 1 - Catches are actually matches!
  try {

    //code
  } catch {

    case r: RuntimeException => "runtime"
    case npe: NullPointerException => "null pointer"
  }


  // Big Idea 2 - all generators are based on pattern matching
  val testList = List(1,2,3,4)
  val evenOnes = for {

    x <- testList if x % 2 == 0
  } yield 10 * x

  val tuples = List((1,2), (5,6))
  val operateTuples = for {

    (first, second) <- tuples // can name the tuples, the same as in pattern matching
  } yield first * second


  // Big Idea 3 - can name multiple variables by exploiting the PM name binding property
  val (a1, b, c) = Tuple3(3,4,5)
  val head :: tail = List(1,2,3,4) //lot of pattern matching goes under the hood to support this


  // Big Idea 4 - Partial
  // the two things below are exactly the same; the top one is known as a partial function literal
  val pf = List(1,2,3,4).map {

    case v if v % 2 == 0 => "even"
    case 1 => "the one"
    case _ => "other"
  } //this syntax is called a partial function

  val sameAsAbove = List(1,2,3,4).map { x => x match {

    case v if v % 2 == 0 => "even"
    case 1 => "the one"
    case _ => "other"
  }}


  // pattern matching allows you to decompose values that conform to a pattern
  val numbers = List(1)
  val numbersMatch = numbers match {
    case head :: Nil => "list with one element"
  }

  /* values available to pattern matching
  - constants
  - wildcards
  - case classes
  - tuples
  - some special magic (like above)
   */

  // let's say you have a non-case class; let's say you can't make it a case class but you want to decompose the members
  class Person2(val name: String, val age: Int)
  // you need to create a companion object with an implemented unapply method exactly like this
  /*
  how does this work? when you run code, runtime says "this is a pattern called person with a `n` and `a`"
  look for a method unapply in an object called person, and that returns a tuple with 2 things
  this returns a `Some`; is this thing empty? it's not; therefore pattern matches
  if you were to modify the unapply such that it returns None, pattern will not match
   */
  object Person {
    def unapply(person: Person): Option[(String, Int)] = Some((person.name, person.age))
  }

  val bobby = new Person2("bob", 42)

  // is there an object called `Person` that has an unapply method which takes the `type of bobby` and returns a tuple of two items
  bobby match {
    case Person(n, a) => s"Hi my name is $n and I am $a years old"
  }

  // can also overload the unapply method in trippy ways
  // Note: the OUTPUT of unapply is what is matched on in case statement; e.g. here runtime looks for unapply[Int,String] and will match on that
  // "do I have an object called Person with an unapply that is Int => String? Yes I do; does it return `Some`? Yes, so match"
  object Person {
    def unapply(person: Person): Option[(String, Int)] = Some((person.name, person.age))

    def unapply(age: Int): Option[String] = Some(if (age < 21) "minor" else "major") // returns `status`
  }

  val legalStatus = bobby.age match {
    case Person(status) => s"my legal status is $status"
  }

  /*
  Define our own patterns
  - when is this useful: when you have to re-use your pattern matching conditions
   */
  object even {
    def unapply(arg: Int): Option[Boolean] =
      if (arg % 2 == 0) Some(true)
      else None
  }

  object singleDigit {
    def unapply(arg: Int): Option[Boolean] =
      if (arg < 10) Some(true)
      else None
  }

  val n = 45
  n match {
    case even(_) => "even"
    case singleDigit(_) => "single digit"
    case _ => "no property"
  }
  // simpler; remove option
  object even {
    def unapply(arg: Int): Boolean = arg % 2 == 0
  }

  object singleDigit {
    def unapply(arg: Int): Boolean = arg < 10

  }

  // take out underscore here, if not using options
  val n = 45
  n match {
    case even() => "even"
    case singleDigit() => "single digit"
    case _ => "no property"
  }

  /**infix patterns**/
  case class Or[A, B](a: A, b: B) // Either implementation; our case class has built in infix
  val either = Or(2, "two")
  val humanDescription = either match {
      case Or(number, string) => s"$number is written as $string"
      case number Or string => s"$number is written as $string" // works exactly the same as above
  }

  /** decomposing sequences */
  val vararg = numbers match {
    case List(1, _*) => "first element is one"
    case 1 :: List(_*) => "first element is one" //works exactly like first one
  }

  // how about our own collection?
  abstract class MyList[+A] {
    def head: A = ???
    def tail: MyList[A] = ???
  }
  case object Empty extends MyList[Nothing]
  case class Cons[+A](override  val head: A, override val tail: MyList[A]) extends MyList[A]

  // to pattern match on our own thing, need to define an unapply sequence method in a companion object to our list
  object MyList {
    // it turns MyList[A] into a Option[Sequence[A]] in which the elements are in the same order
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] =
      if (list == Empty) Some(Seq.empty)
      else unapplySeq(list.tail).map(list.head +: _)
  }

  // compiler looks for object myList that has an unapplySeq method defined, which takes MyList as an arguement and returns an Option[Seq[A]]
  // at runtime, values 1, 2, _* will get matched against what is actually returns by unapplySeq
  val myList: MyList[Int] = Cons(1, Cons(2, Cons(2, Empty)))
  val decomposed = myList match {
    case MyList(1, 2, _*) => "starting with 1 and 2"
  }


  /**custom return types for unapply (not really used in practice)**/
  // return type for unapply; turns out that return type for unapply or unapplySeq doesn't necessarily need to be Option
  // data structure that you use as a return type only needs to have two defined methods: isEmpty (which returns Boolean), get (which returns something)
  abstract class Wrapper[T] {
    def isEmpty: Boolean
    def get: T
  }

  object PersonWrapper {
    def unapply(person: Person): Wrapper[String] = new Wrapper[String] {
      def isEmpty = false
      def get = person.name
    }
  }

  bobby match {
    case PersonWrapper(n) => s"This person's name is $n"
  }
}


























