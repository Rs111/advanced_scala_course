object ImplicitsIntro extends App {

  // have you ever wondered how this compiles?
  // we know it is Daniel.->(555); but there is no -> method on string class (or any other class)
  // -> is an implicit method; a method of an implicit class
  val pair = "Daniel" -> "555"

//  implicit class ArrowAssoc[A](private val self: A) extends AnyVal {
//    def -> = ???
//  }
  /*
    Implicit key word
    - allows for some kind of magic that will turn the first arguement here  (i.e. Daniel) into ArrowAssoc instance
    - it will then call the -> method on it in order to return a tuple
    -
   */

  // example
  case class Person(name: String) {
    def greet = s"Hi my name is $name"
  }

  implicit def fromStringToPerson(str: String): Person = Person(str)

  // you would expect this to fail
  // however: compiler looks for all implicit values/classes/etc that can help in the compilation
  // looks for anything that can turn that String into an instance that has a greet method
  // this way it will find the implicit function
  // compiler assumes there is only one implicit def/class to make this work; it it finds > 1 implicit the code will not compile
  println("Peter".greet) //what compiler does: println(fromStringToPerson("Peter").greet)


  // implicit parameters: defaultAmount will be implicitely passed by the compiler as the second parameter list
  // Note: this is NOT the same thing as default arguements because the implicit value is found by the compiler from its search scope
  
  implicit val defaultAmount = 2
  def increment(x: Int)(implicit amount: Int) = x + amount

  increment(2)
  increment(2)(5)


}
