object CurriesPAF extends App {

  // curried functions: functions which return other functions as results
  val superAdded: Int => Int => Int = x => y => x + y
  val add3 = superAdded(3) // Int => Int = y => 3 + y
  superAdded(3)(5)

  def curriedAdder(x: Int)(y: Int): Int = x + y // curried method
  val add4: Int => Int = curriedAdder(4) // this works with type annotation
  val add42 = curriedAdder(42) // this doesn't work; compiler complains that we are attempting to call method with fewer parameter lists than compiler expects
  // type annotation in `add4` tells compiler that what I'm looking for is the remainder function

  /* lifting: transforming methods to functions (more technical term is called ETA-EXPANSION)
    - ETA-EXPANSION performed by compiler to transform methods into functions
    - the above behind the scenes work is called `lifting`
    - original problem: because curriedAdder is a `method`, def, when you call method you need to call with all parameter lists
    - what we're actually doing with `add4` is we're converting a METHOD to a FUNCTION VALUE of type Int => Int
    - we want to use FUNCTION VALUES in higher order functions
    - can't use methods in higher order functions unless they are transformed into function values
    - this is actually a limitation of the JVM; methods are actually part of instances of classes
      - in this case, curriedAdder is part of the `CurriesPAF` object
    - so methods are not instances of function X themselves
    - tran
  */

  def inc(x: Int): Int = x + 1
  List(1,2,3).map(inc) // ETA-EXPANSION happens here; compiler converts it to map(x => inc(x))


  // Partial Function Applications: the way that we can force the compiler to do ETA expansion when we want is when we want to use PFA

  // turn method into a function value after you've applied the first parameter list
  val add5 = curriedAdder(5) _

  // Excercises - 1
  val simpleAddFunction = (x: Int, y: Int) => x + y
  def simpleAddMethod(x: Int, y: Int): Int = x + y
  def curriedAddMethod(x: Int)(y: Int): Int = x + y

  //add7: Int => Int = y => 7 + y
  val add7One = (y: Int) => simpleAddFunction(7, y)
  val add7One_2 = simpleAddFunction.curried(7)
  val add7Two = (y: Int) => simpleAddMethod(7, y)
  val add7Three = curriedAddMethod(7) _ //PAF
  val add7Three_2 = curriedAddMethod(7)(_) // alternat syntax for the above
  val add7_5 = simpleAddMethod(7, _: Int)


  // underscores are powerful
  def concatenator(a: String, b: String, c: String): String = a + b + c

  val fillInBlanks = concatenator("Hello ", _: String, _: String)
  val insertName = concatenator("Hello, I'm ", _: String, ", how are you?")
  println(insertName("Daniel"))

  // excercise - 2
  def curriedFormatter(s: String)(number: Double): String = s.format(number)
  val numbers = List(Math.PI, Math.E, 1, 9.8, 1.3e-12)
  val simpleFormat = curriedFormatter("%4.2f") _  //lift
  val seriousFormat = curriedFormatter("%8.6f") _
  val preciseFormat = curriedFormatter("%14.12f") _

  println(numbers.map(simpleFormat)) // eta expansion

  // excercise -3: difference between functions vs methods, by-name vs 0-lambdas
  def byValue(n: Int): Int = n + 1 //
  def byName(n: => Int): Int = n + 1 // takes an input that is only evaluated when used
  def byFunction(f: () => Int) = f() + 1

  def method: Int = 42
  def parenMethod(): Int = 42

  // TLDR: byName takes an `evaluated` value; if it is passed a function it will evaluate it first
  byName(23) // okay
  byName(method) // okay (method will be evaluated when called)
  byName(parenMethod()) // okay (same as above)
  byName(parenMethod) // okay (but beware, the paren method is actually called); equivalent to the above
  // in the above, we're not actually doing higher-order stuff;  methods are being evaluated first
  // people confuse byName parameter with higher order function
  // byName is not higher order; it calls method and uses that value
  // byName(() => 42) // doesn't work
  // byName((() => 42())

  // TLDR:
  // byFunction(45) //not okay
  // byFunction(method) // not okay!!!! method here is actually evaluated to its value, and can't be used as function
  // compiler does not do eta expansion in the case above, when the function has no parentheses
  byFunction(parenMethod) // compiler does ETA expansion; this works
  byFunction(() => 46) // works as expected
  byFunction(parenMethod _) // works, but compiler will tell you the _ is not needed


}
