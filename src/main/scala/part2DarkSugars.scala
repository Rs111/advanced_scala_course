import scala.util.Try

object DarkSugars extends App {

  // syntax sugar #0: method with single param
  class Animal {
    def eats(food: String): String  = s"nom nom $food"
  }

  val anAnimal = new Animal
  anAnimal eats "cheese"

  // syntax sugar #1: methods with single parameter
  def singleArgMethod(arg: Int): String = s"$arg little ducks"

  val desc = singleArgMethod {
    // write expression
    45 + 50
  }

  // we often do this with the Try apply method
  val aTryInstance = Try {
    throw new RuntimeException
  }

  //..and with map
  List(1,2,3).map { x =>
    x + 1
  }

  // syntax sugar #2, single abstract method: instances of traits with a single method can actually be reduced to lambdas
  // trait with one method == lambda (this is more clear in example 2)
  trait Action {
    def act(x: Int): Int
  }

  // example 1: normal way to instantiate this trait would be to extend the trait with non-abstract class or implement an anonymous class
  class HasAction extends Action {
    override def act(x: Int): Int = x + 1
  }
  val anInstance: Action = new Action {
    override def act(x: Int): Int = x + 1
  }

  val aFunkyInstance: Action = (x: Int) => x + 1

  // example2 : instantiating traits with Runnable; Runnables are instances of a trait or of a java interface that can be passed on to threads
  val aThreadBad = new Thread(new Runnable {
    override def run(): Unit = println("Hello Scala")
  })
  val aThread = new Thread(() => println("Hello Scala"))

  // example 3: this pattern also works for abstract classes that have some members/methods implemented but only have one method unimplented
  abstract class AnAbstractType {

    def implemented: Int = 23
    def f(a: Int): Unit
  }

  val anAbstractInstance: AnAbstractType = (a: Int) => println(s"a")


  // syntax sugar #3: the :: and #:: methods are special
  val prependList = 2 :: List(3, 4) // compiles as List(3,4).::(2)

  // scala: last character decides the associativity of a method; if it ends in a colon, it means it's right associative (i.e. is a method call on the thing to the right)
  val prependListTwo = 0 :: 1 :: 2 :: List(3, 4) //because operator is right associative, this actually works

  class MyStream[T] {
    def -->:(value: T): MyStream[T] = this
  }

  val myStream = 1 -->: 2 -->: new MyStream[Int]


  // syntax sugar #4: multi-word method naming

  class TeenGirl(name: String) {
    def `and then said`(gossip: String): Unit = println(s"name said $gossip")
  }

  val lilly = new TeenGirl("lilly"); lilly `and then said` "Scala is so sweet"


  // syntax sugar #5: infix types
  class Composite[A, B]
  val composite: Composite[Int, String] = new Composite[Int, String]
  val compositeInfix: Int Composite String = new Composite[Int, String]

  class -->[A, B]
  val towards: Int --> String = ???


  // syntax sugar #6: update() method is special, like apply
  val anArray = Array(1,2,3)
  anArray(2) = 7 // rewritten to anArray.update(indexOfModification, value)


  // syntax sugar #7: setters for mutable containers
  // want to create  mutable wrapper over Int
  class Mutable {
    private var internalMember: Int = 0 // private for )) encapsulation
    def member = internalMember // getter
    def member_=(value: Int): Unit = internalMember = value // setter
  }

  val aMutableContainer = new Mutable
  val aMutableContainer.member = 42 // re-written as aMutable.member_=(internalMember = 42)
}
