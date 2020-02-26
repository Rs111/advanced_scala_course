
import java.{util => ju}

object ScalaJavaConversions extends App {

  // collections are a pain point
  // e.g. scala list very different than scala list

  // this library holds a bunch of implicit defs
  // implicit defs are discouraged, but they are your only option here
  import collection.JavaConverters._

  // e.g.

  val javaSet = ju.Set[Int] = new ju.HashSet[Int]()
  (1 to 5).foreach(javaSet.add)
  println(javaSet)

  //javaConverters => DecorateScala => def asScalaSetConverter(ju.Set[A]): AsScala[mutable.Set[A]]
  // AsScala class has the apply method "asScala: A = op", with op being the scala set
  // therefore: compiler checks for asScala methods on java set; doesn't find any
  // it then checks for anything that can turn a java set into a type with the asScala method
  // finds the asScalaSetConverter; applies it on ju.set to turn it in AsScala, after which the "asScala" method is called
  val scalaSet = javaSet.asScala

  /* converters for
    Ierator
    Iterable
    ju.list -> scala.mutable.Buffer
    ju.Set -> scala.mutable.Set
    ju.Map -> scala.mutableMap
      ...
   */

  /****Scala to java conversion for mutable collections ****/
  import collection.mutable._
  val numbersBuffer = ArrayBuffer[Int](1, 2, 3)

  // also through implicit wrapper
  val juNumbersBuffer = numbersBuffer.asJava

  // the below gives back the same object; i.e. the reference is true
  println(juNumbersBuffer eq numbersBuffer) // gives true

  // converting the below doesn't work out the same
  val numbers = List(1,2,3)
  val numbers = numbers.asJava
  val backToScala = numbers.asScala
  backToScala eq numbers  // gives false
  backToScala == numbers  // gives true, they are the same numbers

  //watch out for:
  numbers.add(7) // won't throw compiliation error, but it will give runtime error
  // numbers is supposed to be immutable, but java doesn't know that


  /*
    Create a Scala-to-Java from Optional to a Scala Option
   */

  class ToScala[T](value: => T) {
    def asScala: T = value
  }

  implicit def asScalaOptional[T](o: ju.Optional[T]): ToScala[Option[T]] = {
    new ToScala[Option[T]](
      if(o.isPresent) Some(o.get)
      else None
    )
  }

  val juOptional: ju.Optional[Int] = ju.Optional.of(2)
  val scalaOption = juOptional.asScala


















}
