package typesystem

object SelfTypes extends App {

  // self types are a way of requireing a type to be mixed in

  // lets say we want to design an API for an awesome band in which every singer must also know how to play an instrument
  // enforce this constraint

  trait Instrumentalist {
    def play(): Unit
  }

  // whoever implements singer must implement Instrumentalist as well
  // all singers must also be instrumentalists
  trait Singer { self: Instrumentalist => // can name is anything; can name it `this: Instrumentalist =>`
    def sing(): Unit
  }

  // how is this useful? we are telling downstream that they MUST implement `play`
  class LeadSinger extends Singer with Instrumentalist {
    override def play(): Unit = ???
    override def sing(): Unit = ???
  }

  // this doesn't work
  class Vocalist extends Singer {
    override def sing(): Unit = ???
  }

  // valid
  val jamesHetfield = new Singer with Instrumentalist {
    override def play(): Unit = ???
    override def sing(): Unit = ???
  }

  // valid
  class Guitarist extends Instrumentalist {
    override def play(): Unit = ???
  }
  val ericClapton = new Guitarist with Singer {
    override def sing(): Unit = ???
  }

  /*** question: why don't we just have Singer extend Intrumentalist? ***/
  // because we want to keep them as two seperate concepts logically in our code
  // we don't want logically that a Singer is a kind of Instrumentalist
  class A
  class B extends A // B is an A

  trait T
  trait S {self: T => } // S requires T

  /*** self types are used for CAKE PATTERN (this is scala's equivalent to "dependency injection") ***/
  // it is called "dependency injection" because with a self-type we enforce a dependency on a type which will then be sub-classed/"injected" later
  // called cake pattern because dependencies create layers of abstraction, which get implemented at the very last stage of the code
  // this is like baking a cake

  // classic dependency injection
  class Component { // reusable component
    // API + code
  }
  class ComponentA extends Component
  class ComponentB extends Component
  // can receive either component to compose our application; injected at Runtime
  class DependantComponent(val component: Component)


  // scala dependency injection (CAKE PATTERN)
  // it is better than regular dependency injection because the above is checked at runtime while our dependencies are checked at compile time
  trait ScalaComponent {
    //API
    def action(x: Int): String
  }
  // self-typing allows me to call the action method from the component as if it were my own
  trait ScalaDependantComponent {
    self: ScalaComponent =>
    def dependantAction(x: Int): String = action(x) + " this rocks"
  }

  trait ScalaApplication {
    self: ScalaDependantComponent =>

  }

  // how does the cake pattern create abstraction layers?
  // EG you are backend dev for a server-side rendering app that displays some profiles and dashboards
  // so you would have to model how various elements on your page/front-end would look like on the backend

  //layer 1 - want to make sure all small components have some common elements (e.g. action)
  trait Picture extends ScalaComponent
  trait Stats extends ScalaComponent

  // layer 2
  // the point: at each layer you can choose what components from the previous layer you want to mix in
  // ScalaDependantComponent requires ScalaComponent to be mixed in; it is supplied by Picture
  trait Profile extends ScalaDependantComponent with Picture
  trait Analytics extends ScalaDependantComponent with Stats

  // layer 3
  trait AnalyticsApp extends ScalaApplication with Analytics


  /*** cyclical dependencies with self types ***/
  // this would not compile because it is cyclical dependency
  class X extends Y
  class Y extends X

  // with self-types it is possible
  // though it may seem like it, there isn't actually a contraction here
  // "whoever implements R must implement S, and whoever implements S must implement R"
  // we are saying R and S are "sister concepts"; they don't extend eachother, but they go hand-in-hand
  trait R { self: S =>}
  trait S { self: R =>}




}
