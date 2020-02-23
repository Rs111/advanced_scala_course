object MagnetPattern extends App {

  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global

  // Magnet pattern is a use-case of type classes
  // aims at solving some of the problems created by method overloading

  // EG working on an API for a remote peer-to-peer communication protocol
  // all the actors in your network are identical so they should be able to handle various kinds of messages

  class P2PRequest
  class P2PResponse
  class Serializer[T]
  trait Actor {
    def receive(statusCode: Int): Int
    def receive(request: P2PRequest): Int
    def receive(request: P2PResponse): Int
    def receive[T : Serializer[T]](message: T): Int // def receive[T](message: T)(implicit serializer: Serializer[T])
    def receive[T : Serializer[T]](message: T, statusCode: Int): Int
    def receive(future: Future[P2PRequest]): Int  // receive something asychronously
    // lots of overloads
  }

  /* poses a lot of problems
    1 - type erasure
        - generic types are erased at compile time, so after type eraser all higher-kinded types look the same
        - type erasure means you cannot overload for higher-kinded types
        - e.g. can't write: def receive(future: Future[P2PResponse]): Int  because we already have a Future[Request]
    2 - if we want higher order functions, lifting doesn't work for all overloards
        - if we tried "val receiveFV = receive _"
        - the "_" is ambigous, compiler will be confused
    3 - code duplication
        - logic for most of the receive methods will be very similar, so implementing them all will involve some duplication
    4 - limitations with default arguements and type inference on arguements
        - imagine of some of the receive methods had default arguements
        - so you could do actor.receive()?!
        - but what would the default arg be? what if multiple of the methods had default args?
        - compiler doesn't know which method/arg to fetch
   */

  // good news: API can be re-written
  trait MessageMagnet[Result] {
    def apply(): Result
  }

  def receive[R](magnet: MessageMagnet[R]): R = magnet()

  implicit class FromP2PRequest(request: P2PRequest) extends MessageMagnet[Int] {
    override def apply(): Int = {
      // holds exact logic for handling a P2P request
      println("handling P2P request")
      42
    }
  }

  implicit class FromP2PResponse(request: P2PResponse) extends MessageMagnet[Int] {
    override def apply(): Int = {
      // holds exact logic for handling a P2P response
      println("handling P2P response")
      24
    }
  }

  // compiler sees it's not a MessageMagnet
  // compiler looks to see if there is anything that converts a P2P to a magnet
  // finds implicit class
  // only our single receive method is only called; acts as a center of gravity for our overloads
  // compiler tries to match the arguement (e.g. P2PRequest) against a magnet; but these types are different, so compiler then looks for implicit conversions
  // magnet pattern: having our overloads in different implicit classes
  receive(new P2PRequest)
  receive(new P2PResponse)

  /*
    magnet pattern has benefits and drawbacks

    benefits:
    1 - no more type erasure problems
        - this works because compiler looks for implicit conversions before the types are erased
    2 - lifting
   */
    // benefit:  1 - no more type erasure problems
  implicit class FromResponseFuture(future: Future[P2PResponse]) extends MessageMagnet[Int] {
    override def apply(): Int = 2
  }

  implicit class FromRequestFuture(future: Future[P2PRequest]) extends MessageMagnet[Int] {
    override def apply(): Int = 3
  }

  println(receive(Future(new P2PResponse)))
  println(receive(Future(new P2PRequest)))

    // benefit: 1 - lifting works
  trait MathLib {
      def add1(x: Int) = x + 1
      def add1(s: String) = s.toInt + 1
      // plus many more overloads
    }

  // magnetize
  // note: not putting a type parameter here, that's very important
  // we did not add the type parameter because if we did, the compiler would not be able to know for which type the lifted function applies to
  // the difference is because we know the result type of `AddMagnet`; it has to be Int; whereas receive has a return value of "Result"
  // compiler does not know what is this value Result
  trait AddMagnet {
    def apply(): Int
  }

  def add1(magnet: AddMagnet): Int = magnet()

  implicit class AddInt(x: Int) extends AddMagnet {
    override def apply(): Int = x + 1
  }

  implicit class AddString(s: String) extends AddMagnet {
    override def apply(): Int = s.toInt + 1
  }

  val addFV = add1 _
  println(addFV(1))
  println(addFV("3"))

  val receiveFV = receive _ // gives us MagnetPattern.MessageMagnet[Nothing] => Nothing


  /* Drawbacks
  - super verbose and harder to read; what the hell is a magnet?
  - can't name or place default arguement
  - call by name doesn't work correctly
   */

  class Handler {
    def handle(s: => String): Unit = {
      println(s)
      println(s)
    }
    // other overloads
  }

  trait HandleMagnet {
    def apply(): Unit
  }

  def handle(magnet: HandleMagnet) = magnet()

  implicit class StringHandle(s: => String) extends HandleMagnet {
    override def apply(): Unit = {
      println(s)
      println(s)
    }
  }

  def sideEffectMethod(): String = {
    println("Hello, Scala")
    "Hahaha"
  }

  handle(sideEffectMethod()) // works as expected
  handle { // this prints hello scala once, and returns haha twice; only the string value is converted to magnet class (be extremely careful)
    println("Hello, Scala")
    "Hahaha"
  }

}
