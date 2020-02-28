package typesystem

object TypeMembers extends App {

  class Animal
  class Dog extends Animal
  class Cat extends Animal

  class AnimalCollection {
    // abstract types
    type AnimalType // abstract type member
    type BoundedAnimal <: Animal
    type SuperBounded >: Dog <: Animal // lower bounded in dog but upper bounded in Animal
    type AnimalC = Cat // this is a type alias; a different name for an existing type

    // abstract types are mostly to help compiler do some type inferences for us
    // won't see them too much in practice
  }

  val ac = new AnimalCollection
  // compiler doesn't complain here, but this line is useless; AnimalType is an abstract type with no constructor
  // can;t construct anything of ac.AnimalType
  val dog: ac.AnimalType = ???

  // even if we have sound bounded type like BoundedAnimal, you cannot associate it with a new Cat
  // we don't know what bounded animal is; it might be a crocodile for all we know
  // because compiler doesn't know what a bounded animal is, it won't compile the code
  val cat: ac.BoundedAnimal= new Cat // compiler doesn't let this happen

  // the compiler DOES allow us to use super bounded abstract types and aliases
  val dog: ac.SuperBounded = new Dog
  val cat: ac.AnimalC = new Cat

  // type aliases used a lot when you have type naming collisions
  type CatAlias = Cat
  val anotherCat: CatAlias = new Cat


  /*** abstract type members are sometimes used in APIs that look similar to generics ***/
  // this type of code is basically an alternative to generics
  trait MyList {
    type T
    def add(element: T): MyList
  }

  class NonEmptyList(value: Int) extends MyList {
    override type T = Int
    def add(element: Int): MyList = ???
  }


  /*** .type; we can use some values type as a type alias ***/
  type CatsType = cat.type
  val newCat: CatsType = cat

  // but you can't do this; compiler can't really find if it's constructable or not
  new CatsType


  /*
    Exercise - enforce a type to be applicable to SOME TYPES only
   */

  // this is locked, someone else wrote it (e.g. different team)
  trait MList {
    type A
    def head: A
    def tail: MList
  }

  // but you think the design is bad, should only be applicable for Ints
  // this list below works but you don't want it to
  class CustomList(hd: String, tl: CustomList) extends MList {
    type A = String
    def head: A = hd
    def tail: CustomList = tl
  }

  // you want this one to be okay, but the above to not be okay
  class IntList(hd: Int, tl: IntList) extends MList {
    type A = Int
    def head: A = hd
    def tail: IntList = tl
  }

  // enforce at compile time that the other one doesn't compile
  trait ApplicableToNumbers {
    type A <: Number
  }

  // if mixing in with the trait, it enforces that type A defined in sub-classes but be sub Number
  class IntList2(hd: Int, tl: IntList2) extends MList with ApplicableToNumbers {
    type A = Int
    def head: A = hd
    def tail: IntList2 = tl
  }













}
