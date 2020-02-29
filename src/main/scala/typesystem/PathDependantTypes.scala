package typesystem

object PathDependantTypes extends App {

  /*** this lesson is about nested types and about how they are used and accessed ***/
  // Outer class has an inner class
  class Outer {
    class Inner
    object InnerObject
    type InnerType

    def print(i: Inner) = println(i)
    def printGeneral(i:  Outer#Inner) = println(i)
  }

  // for the most part, can define anything anywhere
  // the exception is Types: anywhere other than classes & traits you can only define them as aliases
  def aMethod: Int = {
    class HelperClass

    type HelperType = String // compiler expects you to write alias here

    2
  }

  // how to use inner stuff
  // in the case of things like inner-classes and inner-objects, the class/object/trait members are defined per-instance
  //EG
  val outer = new Outer
  val inner = new Inner // cannot do this, inner exists only in the context of outer
  val inner2 = new Outer.Inner //can also not say this
  val inner3: outer.Inner = new outer.Inner // can do this: o.inner is a TYPE

  // in order to reference an inner type, you need an outer instance; different instances will mean different inner types
  val outer2 = new Outer
  val inner4: outer2.Inner = new outer.Inner //cannot assign `outer.Inner` to outer2.Inner; they are unrelated types
  outer.print(new outer.Inner) // works
  outer.print(new outer2.Inner) // does not work


  // the above is what we call path-dependant types

  // Another take-away: all the inner types have a common super type (Outer#Inner)
  outer.printGeneral(new outer2.Inner) //this works


  /*
    Exercise
      - you are the developer of a small database
      - database may be keyed by Integers or Strings
      - you want to design it such that you can expand it in the future if needed
      - in your current design you have Item, IntItem, StringItem
   */

  trait Item1[Key]
  trait IntItem1 extends Item1[Int]
  trait StringItem1 extends Item1[String]

  //ItemType has a key generic type as well
  def get1[ItemType1](key: Key): ItemType1

  get1[IntItem1](42) // this should compile
  get1[StringItem1]("home") // this should compile
  get1[StringItem1](5) // this should not compile


  // SOLUTION
  // we basically want type constraints
  // one possible solution is to mix in trait

  trait ItemLike {
    type Key
  }

  // enforce that type key here is equal to the type K that this item is parameterized with
  trait Item[K] extends ItemLike {
    type Key = K
  }

  // upper bounded tells compiler: ItemType is type parameterized with something that has an abstract type member
  // this abstract type member can be used as a path dependant type by saying ItemType#Key
  def get[ItemType <: ItemLike](key: ItemType#Key): ItemType

  get[IntItem1](42) // this should compile
  get[StringItem1]("home") // this should compile
  get[StringItem1](5) // this should not compile
  get(5) // doesn't work


}
