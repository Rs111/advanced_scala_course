object PimpMyLibrary extends App {

  // implicits allow us to add methods to libraries that we don't have access to via implicit classes

  // implicit classes must take only one arguement
  implicit class RichInt(val value: Int) extends AnyVal { // extending AnyVal adds memory optimization bonuses
    def isEven: Boolean = value % 2 == 0

    def sqrt: Double = Math.sqrt(value)

    def times(function: () => Unit): Unit = {
      def timesAux(n: Int): Unit =
        if (n <= 0) ()
        else {
          function()
          timesAux(n - 1)
        }
    }

    def *[T](list: List[T]): List[T] = {
      def concatenate(n: Int): List[T] =
        if (n <= 0) List()
        else concatenate(n - 1) ++ list

      concatenate(value)
    }
  }

    // type enrichment = pimping
    // when you type '42.isEven' the compiler interprets it as an error in the first place
    // but then it searches for all implicit classes (or all implicit conversions including classes with implicit defs) that can wrap a type Int into something that contains the method isEven
    // the compiler than inserts an invisible constructor call
    new RichInt(42).sqrt
    42.isEven // read as new RichInt(42).isEven

    // restriction: compiler does not do multiple implicit searches
    implicit class RicherInt(richInt: RichInt) {
      def isOdd: Boolean = richInt.value % 2 == 0
    }

    // 42.isOdd // this line does not work

    /*
  Enrich the String class; you should/can not do the below
   */

    implicit class RichString(s: String) {
      def asInt: Int = Integer.valueOf(s)

      // caesar cypher
      def encrypt(cypherDistance: Int): String = s.map(c => (c + cypherDistance).asInstanceOf[Char])
    }

 // every programmer will hate you
    // compiler attempts to call "/" method on a string
    // sees string has no "/" method
    // compiler looks for all classes, wrappers, conversions that given a String return something that has the "/" method
    // then it finds the implicit below and automatically applies to string
    implicit def stringToInt(string: String): Int = Integer.valueOf(string)

    println("3" / 5) // that makes this line work; compiles as stringToInt("3")./(5)

    // this is equivalent
    class RichAltInt(value: Int)
    implicit def enrich(value: Int): RichAltInt = new RichAltInt(value)

    // conversions with methods are more powerful, but are also discouraged
    implicit def intToBoolean(i: Int): Boolean = i == 1

    /*
    goal:
      if (n) do something
      else do something else
     */

    val aConditionValue = if (3) "OK" else "something wrong"
  // prints something wrong; in situation where
  // compiler looks for something that will turn 3 into a boolean; finds the implicit def which returns false
  //DANGER ZONE: if there is a bug in an implicit conversion due to a method it is mega hard to trace it back
  //Implicit methods tend to be part of libraries which are in a different part of the code base
    println(aConditionValue)




















}
