object OrganizingImplicits extends App{

  // how do we store implicits in ways that don't confuse the compiler?
  // we'll practice with ordering, which is a thing that takes implicit parameters
  // sorted method takes an implicit ordering; i.e. there is already an implicit ordering for List[Int]
  // scala stores it in scala.Predef (it is imported automatically when you write code)
  println(List(1,4,5,3,2).sorted)

  // if you define an implicit val, it will be used instead of the one in scalaPredef
  // can also just pass it to function, whether ordering is implicit or not
  // if you were to define another implicit, code will not compile correctly
  implicit val reverseOrdering: Ordering[Int] = Ordering.fromLessThan(_ > _)
 // implicit val normalOrdering: Ordering[Int] = Ordering.fromLessThan(_ < _)
  println(List(1,4,5,3,2).sorted)
  println(List(1,4,5,3,2).sorted(reverseOrdering))

  // this lesson is about how/where compiler looks for implicits and which implicits have priority

  /*
    Implicits (used as implicit parameters):
      - val/var
      - object
      - accessor methods = defs with no parentheses
   */

  // works
  implicit def reverseOrdering2: Ordering[Int] = Ordering.fromLessThan(_ > _)
  // doesn't work
  implicit def reverseOrdering3(): Ordering[Int] = Ordering.fromLessThan(_ > _)

  // exercise
  case class Person(name: String, age: Int)
  val persons = List(
    Person("Steve", 30),
    Person("Amy", 22),
    Person("John", 66)
  )

  // compiler will only have access if implicit is above it's use
  implicit val alphabeticOrdering: Ordering[Person] = Ordering.fromLessThan((a, b) => a.name.compareTo(b.name) < 0)
  persons.sorted

  /*
    Implicit scope (where the compiler searches for implicits)
    - scope is composed of several parts and there are rules that prioritize some parts over others
    - normal scope = LOCAL SCOPE
      - has highest priority
      - this is basically where we write our code; i.e. if you write implicit val in your code, that one will be used
      - note: compiler will not find your implicit if it is in an object
    - imported scope
      - second highest priority
    - companion objects of all types involved in the method signature
      - EG for sorted: def sorted[B >: A](implicit ord: Ordering[B]): List[B]
      - compiler will look for implicit orderings in List, Ordering, all the types involved (A or any supertype)

    Best Practices
    - good practice is to define it in companion object if one implicit satisfies the majority of use cases
    - if the companion implicit needs to be over-ridden, create new implicit in local scope
    - if there are many good possible implicits, package them seperately and make user import the right container
      - e.g. like the AgeOrdering example below
   */

  object AgeOrdering {
    implicit val ageOrdering: Ordering[Person] = Ordering.fromLessThan((a, b) => a.age < b.age)
  }
  import AgeOrdering.ageOrdering


  /*
    Exercise
    - build orderings
    - by total price = most used 50%
    - by unit count = 25%
    - by unit price = 25%
   */

  case class Purchase(nUnits: Int, unitPrice: Double)
  object Purchase {
    implicit val totalPriceOrdering: Ordering[Purchase] = Ordering.fromLessThan((a, b) => a.nUnits * a.unitPrice < b.nUnits * b.unitPrice)
  }
  object UnitCountOrdering {
    implicit val unitCountOrdering: Ordering[Purchase] = Ordering.fromLessThan((a, b) => a.nUnits < b.nUnits)
  }
  object UnitPriceOrdering {
    implicit val unitPriceOrdering: Ordering[Purchase] = Ordering.fromLessThan((a, b) => a.unitPrice < b.unitPrice)
  }

















}
