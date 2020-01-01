object PartialFunctions extends App {
  /*
  Notes
  - PF can only have one parameter type
   */

  // any Int passed (i.e. whole domain) will return a value
  val aFunction = (x: Int) => x + 1 // this is type `Function1[Int,Int]` or Int => Int

  // what if we want a function to only accept 1, 2, 5?
  val aFussyFunction = (x: Int) =>
    if (x == 1) 42
    else if (x == 2) 55
    else if (x == 5) 999
    else throw new FunctionNotApplicableException

  class FunctionNotApplicableException extends RuntimeException

  // less clunky implementation; type is {1,2,5} => Int; is called a partial function
  val aNicerFussyFunction = (x: Int) => x match {
    case 1 => 42
    case 2 => 56
    case 3 => 999
  }

  // equivalent to the above
  // calling a partial function with a value not in it's domain will trigger a match error (subtype of runtime exception)
  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 56
    case 3 => 999
  }

  // another way to write
  val anotherImplementation = new PartialFunction[Int, Int] {
    override def apply(x: Int): Int = x match {
        case 1 => 42
        case 2 => 2727
    }

    override def isDefinedAt(x: Int): Boolean = x == 1 || x == 2
  }

  /**Partial Functions Utilities**/
  // returns bool
  println(aPartialFunction.isDefinedAt(67))

  // Lift: partial functions can be lifted to total functions returning options
  val lifted = aPartialFunction.lift// Int => Option[Int]

  val pfChain = aPartialFunction.orElse[Int, Int] {
    case 45 => 67
  }

  // Partial Functions extend Normal Functions
  val aTotalFunction: Int => Int = {
    case 1 => 99
  }

  // HOFs accept partial functions
  val aMappedList = List(1,2,3). map {
    case 1 => 42
    case 2 => 48
    case 3 => 999
  }
}
