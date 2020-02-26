package typesystem

object RockingInheritance extends App {

  /*** convenience ***/
  // writing an API for an I/O library; writing and reading thing

  // have a writer which writes things of type T
  trait Writer[T] {
    def write(t: T): Unit
  }
  // closeable resources
  trait Closeable {
    def close(status: Int): Unit
  }
  // stream of values that we read from our resources
  trait GenericStream[T] {
    def foreach(f: T => Unit): Unit
  }

  // we have a minimal API for our imaginery library above
  // convenience here is: if we have a generic stream that also mixes in some other traits, we can define a method like process stream
  // if we have an idea about the type we want to receive, like a generic stream that mixes in closeable and writer, but we don't know who exactly does that, we can do the below
  // GenericStream[T] with Writer[T] with Closeable is it's own type; it has access to all our API
  // if we don't know what class mixes in everything we want, we can just make it
  def processStream[T](stream: GenericStream[T] with Writer[T] with Closeable): Unit = {
    stream.foreach(println)
    stream.close(0)
  }

  /*** diamond problem ***/
  trait Animal { def name: String}
  trait Lion extends Animal {override def name: String = "Lion"}
  trait Tiger extends Animal {override def name: String = "Tiger"}
  // perfectly compilable code; inherits def nam {e from both lion and tiger
  class Mutant extends Lion with Tiger {
    override def name: String = "alien"
  }
  // interesting part: code compiles even if you remove override; so what is saving us from the diamond problem?
  class Mutant2 extends Lion with Tiger

  // the below prints out tiger; why?
  /* Mutant2
        extends Animal
         with {override def name: String = "Lion"}
         with {override def name: String = "Tiger"}

     Last override gets picked
     this is how scala resolves the diamond problem
   */
  val m = new Mutant; println(m.name)

  /*** the super problem + type linearization ***/
  // note: super accesses method from a parent class or trait

  trait Cold {
    def print: Unit = println("cold")
  }

  trait Green extends Cold {
    override def print: Unit = {
      println("green")
      super.print
    }
  }

  trait Blue extends Cold {
    override def print: Unit = {
      println("blue")
      super.print
    }
  }

  class Red {
    def print: Unit = println("red")
  }

  class White extends Red with Green with Blue {
    override def print: Unit = {
      println("white")
      super.print
    }
  }

  // what will happen below?
  // prints: white, blue, green cold
  val color = new White; color.print

  /* type hierarchy:
     - cold
      - green -
      - blue  - white

     - red    -

     Cold = AnyRef with <Cold> (<Cold> is body of cold)
     Green = Cold with <Green>
           = AnyRef with <Cold> with <Green>
     Blue = Cold with <Blue>
          = AnyRef with <Cold> with <Blue>
     Red = AnyRef with <Red>

     White = Red with Green with Blue with <White>
           = AnyRef with <Red>
             with AnyRef with <Cold> with <Green>
             with AnyRef with <Cold> with <Blue>
             with <White>
           // compiler jumps over what it sees the second time
           // the below is called a type linearization for White
           = AnyRef with <Red>
             with <Cold> with <Green>
             with <Blue>
             with <White>

     - in the context of type linearization, the super key word gets a whole new meaning
     - if you call super, it will point to the item immediately to the left
     - if you call super again, it will jump one more step
     - e.g. in this example, we called white's print which calls Blue's print; but Blue's print has a super, which calls Green's in this case (it is to the left in linearization)
   */

















}
