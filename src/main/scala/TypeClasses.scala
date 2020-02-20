object TypeClasses extends App {

  /* type class
    - is a trait that takes a type arguement and describes what operations can be applied to that type
    - Ordering trait is an example of this; implicit orderings we defined are type class instances
   */

  // we are backend for social network, and we have decided to do server side rendering
  // all classes that extend this trait will have to implement the toHtml method
  trait HTMLWritable {
    def toHtml: String
  }

  // we have rendered a small user to an HTML by rendering a div and a small link
  case class User(name: String, age: Int, email: String) extends HTMLWritable {
    override def toHtml: String = s"<div>$name ($age yo) <a href=$email/> </div>"
  }

  /*
    the below works, but it has two big disadvantages:
    1 - it only works for types that WE write; for any other types (java standard dates, etc) we would need to write conversions to other types (which are never pretty)
    2 - this is only ONE implementation out of a lot; e.g. might need a different implementation for when user is logged in or not
   */
  val john = User("John", 32, "john@rockthejvm.com")
  john.toHtml

  /*
  option 2: use pattern matching
    - got some benefits but lost a few
    - lost type safety because value can be anything
    - need to modify code everytime we have a new thing we want to render
   */
  object HTMLSerializerPM {
    def serializeToHtml(value: Any) = value match {
      case User(n, a, e) => "a"
      case java.util.Date => "b"
      case _ => "c"
    }
  }

  /*
    best design
    1 - we can define serializers for other types that we may not have written; e.g. DateSerializer
    2 - we can define multiple serializers for a certain type
   */

  trait HTMLSerializer[T] {
    def serialize(value: T): String
  }

  object UserSerializer extends HTMLSerializer[User] {
    override def serialize(value: User): String = s"<div>$value.name ($value.age yo) <a href=$value.email/> </div>"
  }
  UserSerializer.serialize(john)

  // e.g. for people who aren't logged in
  object PartialUserSerializer extends HTMLSerializer[User] {
    override def serialize(value: User): String = s"<div>$value.name </div>"
  }

  /*
    - HTML Serializer is a TYPE CLASS; defines some operations (in this case serialize) that can be applied to a given type
    - all implementers of the type class (e.g. UserSerializer) are called typeclass instances

    Type classes are a notoriously abstract concept. think of them this way:
    - as a data type, a normal class describes a collection of methods and properties that something must have in order to belong to that class
      - e.g. if "John" is of type string, it must support the length operation
      - type checker can use this info at compile time to find possible errors (static type checking)
    - type class, as opposed to a normal class, lifts the concept to Types
      - i.e. it describes a collection of methods and properties that a type must have in order to belong to that specific type class
      - e.g. if it is known that a type belongs to an Ordering type class, then it is known that instances of that class have the ability to compare values
   */

  trait MyTypeClassTemplate[T] {
    def action(value: T): String
  }

  // Equality type class
  trait Equal[T] {
    def apply(a: T, b: T): Boolean
  }

  object NameEquality extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.name == b.name
  }

  object FullEquality extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.name == b.name && a.age == b.age
  }

}
