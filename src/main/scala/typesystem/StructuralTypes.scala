package typesystem

object StructuralTypes extends App {
  // also called compile-time duck typing
  // advanced feature

  // we are going to dissect the notion of type as a structure

  /*** structural types ***/
  // a structural type is basically -> {def ... val}; it can be assigned/aliased using `type` key word
  // imagine on your huge team, someone wrote something with java that you want to use
  // java has this interface (which we will write as a trait) which closes things

  type JavaCloseable = java.io.Closeable // this is a kind of thing that can close; i.e. something with this behavior can be closed

  // your own team has implemented their own closeable
  class HipsterCloseable {
    def close(): Unit = println("yeah yeah im closing")
  }

  // both JavaCloseable and HipsterCloseable have a `close` method which returns `Unit`

  // you want to write a method that safely accept both methods without duplicating the code
  // note: if it's your own code base, could just extend a trait with the abstract method

  def closeQuietly(closeable:JavaCloseable OR HipsterCloseable) // wouldn't this be nice?

  // this is called a STRUCTURAL TYPE; may have values/methods/etc inside
  // what this means: ANY TYPE that has a close method
  // dangerous because there could be un-related types that have a close method; should do some tricks to make sure it only accepts the two intended
  // tricks: TypeMembers + Path Dependant
  type UnifiedCloseable = {
    def close(): Unit
  }

  def closeQuietly(unifiedCloseable: UnifiedCloseable): Unit = unifiedCloseable.close()

  closeQuietly(new java.io.Closeable {
    override def close(): Unit = ???
  })
  closeQuietly(new HipsterCloseable)


  /*** TYPE REFINEMENTS/ENRICHMENT (we're going to use our weapon above) ***/

  // AdvancedClosable is a JavaCloseable plus this extra method
  // We are basically saying: type AdvancedCloseable is ANY JavaCloseable that also implements a closeSilently method
  type AdvancedCloseable = JavaCloseable {
    def closeSilently(): Unit
  }

  class AdvancedJavaCloseable extends JavaCloseable {
    override def close(): Unit = println("java closes")
    def closeSilently(): Unit = println("Java closes silently")
  }

  // we will see how AdvancedJavaCloseable is a good substitute for the type alis AdvancedCloseable

  def closeShh(advancedCloseable: AdvancedCloseable): Unit = advancedCloseable.closeSilently()
  // this works
  closeShh(new JavaCloseable {
    override def close(): Unit = println("")
    def closeSilently(): Unit = println("")
  })
  //this works too, obviously
  closeShh(new AdvancedCloseable)


  /*** Use structural types as stand-alone types ***/
  //{def close(): Unit} is it's own type, without an alias
  // when we say `type ...` we are just assigning/alias-ing something to that name
  def altClose(closeable: {def close(): Unit}): Unit = println("")



  /*** type checking => duck typing (static duck typing) ***/
  // python programmers know what I'm talking about

  type SoundMaker = {
    def makeSound(): Unit
  }

  // from a structural standpoint, Dog and Car both conform to the structure defined by SoundMaker
  class Dog {
    def makeSound(): Unit = println("bark")
  }

  class Car {
    def makeSound(): Unit = println("vroom")
  }


  // from compiler's point of view, you can say the below
  /* this is called static duck typing
    - The idea is that you don't need a type in order to invoke an existing method on an object
    - if a method is defined on it, you can invoke it
    - The name comes from the phrase "If it looks like a duck and quacks like a duck, it's a duck".
      - this phrase is also the "duck test"
    - used in languages like Python that do not have strong types (dynamic languages)

    - In Scala: if an instance conforms to the structure of some stuctural type (e.g. SoundMaker), I can use it as an instance of that type
    - Scala does duck test at compile time

    BIG CAVEAT: structural types, including type refinements, are based on reflection
      - this is how scala compiler is able to guarantee at compile time that an instance will be used as a structural type
      - because at runetime the program will inspect the presence of the `makeSound` method and invoke it
      - advanced JVM developer tidbit: reflective calls have a big impact on performance (in bad way)
      - TAKEAWAY only use structural types when you absolutely need to
   */
  val dog: SoundMaker = new Dog
  val car: SoundMaker = new Car


  /*
    Excercises
   */
  // 1 - try to think of whether f is compatible with CBL and Human
  // Answer - f is compatible with both
  trait CBL[+T] {
    def head: T
    def tail: CBL[T]
  }

  class Human {
    def head: Brain = new Brain
  }

  class Brain {
    override def toString: String = "BRAINS"
  }

  def f[T](somethingWithAHead: { def head: T}): Unit = println(somethingWithAHead.head)

  // proof
  case object CBNil extends CBL[Nothing] {
    def head: Nothing = ???
    def tail: CBL[Nothing] = ???
  }
  case class CBCons[T](override val head: T, override val tail: CBL[T]) extends CBL[T]

  f(CBCons(2, CBNil))
  f(new Human)

  // 2 - is this compatible with a CBL and human?
  // Answer - Yes
  object HeadEqualizer {
    type Headable[T] = { def head: T }
    def ===[T](a: Headable[T], b: Headable[T]): Boolean = {a.head == b.head}
  }
  val brainzList = CBCons(new Brain, CBNil)
  val stringsList = CBCons("Brainz", CBNil)
  HeadEqualizer.===(brainzList, new Human) // head arguement applied to each returns a brain
  // problem: not type-safe: head arguement applied to elements returns different types
  // this happens because the structural equalization (a.head == b.head) relies on reflection
  // because this all relies on reflection, the compiler is able to erase `T` type parameter in `===[T]`
  // this reduces the equals method to a comparison between two headables, without factoring in the type
  // TAKEAWAY: be careful when using structural types in the context of methods with type parameters
  HeadEqualizer.===(new Human, stringsList)

}
