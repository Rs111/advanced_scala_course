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

  // updated this template at end of lessons with all we know
  trait MyTypeClassTemplate[T] {
    def action(value: T): String
  }

  object MyTypeClassTemplate {
    def apply[T](implicit instance: MyTypeClassTemplate[T]): MyTypeClassTemplate[T] = instance

    def action[T](value: T)(implicit myTypeClassTemplate: MyTypeClassTemplate[T]): String =
      myTypeClassTemplate.action(value)
  }

  // Equality type class v1
  trait Equal[T] {
    def apply(a: T, b: T): Boolean
  }

  object Equal {
    def apply[T](a: T, b: T)(implicit equalizer: Equal[T]): Boolean = equalizer.apply(a, b)
  }

  // could do this if either of the objects below were implicits
  println(
    Equal(
      new User("a", 1, "b"),
      new User("a", 1, "b")
    )
  )

  object NameEquality extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.name == b.name
  }

  object FullEquality extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.name == b.name && a.age == b.age
  }


  /*********Now we will provide implicit type class instances ***********/
  // this is called AD-HOC polymorphism
  // can call HTML Serializer on any type we want, so long as implicit is defined
  // this is polymorphism because compiler fetches different object depending on type
  object HTMLSerializer {
    def serialize[T](value: T)(implicit serializer: HTMLSerializer[T]): String =
      serializer.serialize(value)

    def apply[T](implicit serializer: HTMLSerializer[T]) = serializer
  }

  implicit object IntSerializer extends HTMLSerializer[Int] {
    override def serialize(value: Int): String = s"<div style: color=blue>$value</div>"
  }

  implicit object UserSerializer2 extends HTMLSerializer[User] {
    override def serialize(value: User): String = s"<div style: color=blue>${value.name}</div>"
  }

  println(HTMLSerializer.serialize(42)(IntSerializer)) // need to do this if IntSerializer is not implicit
  println(HTMLSerializer.serialize(42)) // can just do this if IntSerializer is implicit
  println(HTMLSerializer.serialize(new User("John", 39, "fakeemail")))

  // using apply
  // advantage of this design is that once we say `HTMLSerializer[User]` we get access to the entire type class interface (e.g. maybe other methods aside from serialize)
  println(HTMLSerializer[User].serialize(john))


  /*******part 3: type enrichment (pimp library ************/
  // in context of type classes, type enrichment will allow us to invoke out type class pattern for any type for which we have an HTMLSerializer

  implicit class HTMLEnrichment[T](value: T) {
    def toHTML(implicit serializer: HTMLSerializer[T]): String = serializer.serialize(value)
  }

  println(john.toHTML(UserSerializer)) // compiled as: new HTMLEnrichment[User](john).toHTML(UserSerializer)
  println(john.toHTML)

  /* The above solves all of our earlier problems:
  - can extend to new types (as long as we have an implict serializer
  - can also implement more than one serializer for a given type; just import or create the implicit you want
  - super expressive
  - is Type safe; e.g. for EqualBetter exercise, compiler will tell you before running that bob == 42 is not valid; compiler prevents you from even compling the code

  our construct now consists of:
  - type class itself (i.e. HTMLSerializer trait, which says we can perform serialize on T)
  - type class instances (some of which are implicit); UserSerializer or IntSerializer
  - conversion with implicit classes (HTML Enrichment); this allows you to use type class instances as implicit parameters
   */

  // context bounds
  // let's say in our backend we have a small method
  def htmlBoilerplate[T](content: T)(implicit  serializer: HTMLSerializer[T]): String = {
    s"<html><body> ${content.toHTML(serializer)}</body></html>"
  }

  // can re-write the above in a nicer way
  // main difference: can't use serializer because `: HTMLSerializer` is a context bound which tells the compiler to inject an implicit parameter of type HTMLSerializer[T]
  // the advantage is the super compact method signature
  // disadvantage is that we can not use serializer by name because the compiler injects it for us
  def htmlSugar[T : HTMLSerializer](content: T): String =
    s"<html><body> ${content.toHTML}</body></html>"

  // can also do this with implicetly
  def htmlSugar2[T : HTMLSerializer](content: T): String = {
    val serializer = implicitly[HTMLSerializer[T]]
    s"<html><body> ${content.toHTML(serializer)}</body></html>"
  }

  // implicitly
  // lets say someone creates default value for permissions for a social media
  case class Permissions(mask: String)
  implicit val defaultPermissions: Permissions = Permissions("0744")

  // in some other part of the code, we want to surface what the default permissions are
  // implicetly has a very simple implementation
  val standardPerms = implicitly[Permissions]


  /****Improve equal type class with an implicit conversion class ****/
  // Equality type class v1
  trait EqualBetter[T] {
    def apply(a: T, b: T): Boolean
  }

  object EqualBetter {
    def apply[T](a: T, b: T)(implicit equalizer: EqualBetter[T]): Boolean = equalizer.apply(a, b)
  }

  implicit object NameEqualityBetter extends EqualBetter[User] {
    override def apply(a: User, b: User): Boolean = a.name == b.name
  }

  object FullEqualityBetter extends EqualBetter[User] {
    override def apply(a: User, b: User): Boolean = a.name == b.name && a.age == b.age
  }

  implicit class EqualBetterEnrichment[T](a: T) {
    def ==(b: T)(implicit equalityBetter: EqualBetter[T]): Boolean = equalityBetter(a, b)
    def !=(b: T): Boolean = !(a == b)
  }

  val bob = new User("bob", 50, "123")
  val bob2 = new User("bob2", 50, "123")

  EqualBetter(bob, bob2)
  bob == bob2
  bob != bob2








}
