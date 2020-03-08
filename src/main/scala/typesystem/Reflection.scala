package typesystem

object Reflection extends App {

  // problem: how do i instantiate a class or invoke a method by calling it's name dynamically at runtime?
  // reflection: ability of the JVM to inspect and operate on classes and instances and call methods at runtime
  // java already offers a reflection API which is available to scala as well
  // but because scala types are richer (e.g. they can have unknown type parameters, abstract type members) it doesn't always give you correct result
  // we will use the scala reflection API
  // scala reflection API + macros + quasiquotes allows scala programs to inspect and modify other scala programs (and even themselves)
  // the concept above is called METAPROGRAMMING

  case class Person(name: String) {
    def sayMyName(): Unit = println(s"Hi, my name is $name")
  }

  /*
    steps to invoke scala reflection API
    - the below allows us to instantiate a dynamically computed class name at runtime (clazz) with some arguements (apply)
   */
  // step 0
  import scala.reflect.runtime.{universe => ru}

  // step 1 - MIRROR; something that can reflect things
  // classloader: an instance of a JVM class that can load other classes at runtime
  // getClass.getClassLoader: is the standard way to get the current class loader; we are getting the class loader for `Reflection`
  val m = ru.runtimeMirror(getClass.getClassLoader) // takes classloader as arguement

  // step 2 - create a class object
  // clazz is a `ClassSymbol` so it's kind of like a "description" of the class
  val clazz = m.staticClass("typesystem.Reflection.Person") // creating a class object by NAME

  // step 3 - create reflected mirror
  // cm, acting on a class, can access members/constructors/methods and can do things (e.g. invoke methods, instantiate objects)
  val cm = m.reflectClass(clazz)

  // step 4 - get constructor
  // dynamically instantiate a person at runtime without actually calling the constructor; we do that at runtime by computing class's name and invoking it's constructor
  val constructor = clazz.primaryConstructor.asMethod

  // step 5 - reflect the constructor
  val constructorMirror = cm.reflectConstructor(constructor)

  // step 6 - invoke the constructor
  val instance = constructorMirror.apply("John")
  println(instance)



  /*
    Another Use-Case
    - I have an instance already computed (p = Person...)
    - It's not something I created, it's something I obtained from somewhere else
      - e.g. from the wire as a serialized object
    - let's say method name is also computed from somewhere else; I want to invoke this
   */

  val p = Person("Mary")
  val methodName = "sayMyName"

  // problem: we don't know the type of `p` and we don't know what the method actually is
  // we can only call the method dynamically
  // 1 - mirror (already done in example one)
  // 2 - reflect the instance instead of the class we want to instantiate; is InstanceMirror
  val reflected = m.reflect(p)
  // 3 - get symbol
  // after `ru.typeOf[Person]` you're getting a `Type` instance
  val methodSymbol = ru.typeOf[Person].decl(ru.TermName(methodName)).asMethod
  // 4 - reflect the method = can DO things
  val method = reflected.reflectMethod(methodSymbol)

  method.apply() // returns My name is Mary


  /* TLDR
    - these are examples of how you would use a member or a method at runtime just by using their name
    - pattern is:
      - instantiate a mirror
      - reflecting whatever it is you would use for invoking the member at runtime
      - e.g. reflect class, reflect instance
      - get ahold of a mirror of the thing you want to use, and then create a symbol of the thing you want to invoke
        - e.g. constructor & method in the two examples
      - then create a reflected version of the thing using the symbol
      - and then you invoke that thing
   */


  /*** reflection in the context of type erasure ***/
  // recap: generic types are erased at compile time
  // the reason is that java must be backwards compatible
  // java5 introduced concept of generics with primary goal og type-checking collections
  // but pre-java 5 that did not exist; so java 5 compiler erased generic types to maintain backward compatibility
  // consequence: all JVM languages that implement some form of generics have to go through the same type erasure process

  /* Two Big Pain Points with Type Erasure*/

  // Pain Point 1: can not differentiate between generic types at runtime
  // code below returns string; at compile time, the generic type is erased (List[String] becomes List)
  val numbers: List[Int] = List(1,2,3)
  numbers match {
    case listOfString: List[String] => "string"
    case listOfInt: List[Int] => "int"
  }

  // Pain Point 2: some limitations on overloads
  // compiler complains because after compilation both methods get reduced to def processList(list: List): Int
  // unless we resort to magnet pattern, then we are stuck with this painpoint
  def processList(list: List[Int]): Int = 42
  def processList(list: List[String]): Int = 43

  /* work-around: scala has a reflection API workaround which involves carrying the complete generic type available at compile time to the runtime*/
  // TypeTags

  // 0 - import everything in ru
  import ru._
  // 1 - create typeTag manually
  // typeTag is a method which takes an implicit ttag and just returns that ttag
  //  def typeTag[T](implicit ttag: TypeTag[T]) = ttag; works like `implicitly` method
  // creating a typeTag is done by the compiler and the information contained in the typeTag is carried to the runtime
  // by accessing the tpe method then I have access to a type instance
  val ttag = typeTag[Person]

  // having access to a typeTag allows us to use whatever has been defined in this type
  println(ttag.tpe) //tpe is the fully qualified classname of person; i.e. the path to the class

  // type tags aren't useful by themselves
  // they are basically used as type evidence which thena llows us to inspect generic types
  class MyMap[K, V]

  // 2 - pass typeTags as implicit parameters; compiler does this for us
  // preferred way of dealing with typeTags is to pass them as implicit parameters
  def getTypeArguements[T](value: T)(implicit typeTag: TypeTag[T]) = typeTag.tpe match {
    case TypeRef(_,_, typeArguements) => typeArguements
    case _ => List()
  }


  // EG: my desire is to have access to Int, String att runtime
  val myMap = new MyMap[Int, String]
  val typeArgs = getTypeArguements(myMap)//(typeTag: TypeTag[MyMap[Int, String]]) // compiler creates the implicit parameter
  // typeTag therefore carries generic type info to the runtime
  println(typeArgs) // returns List(Int, String)
  // result: we have access to the erased type information at runtime, through these little data structures that the compiler creates before compiling the code,...
  // we can do a bunch of checks and operations

  // ttag data structures are filled at compile time with all the information of the class
  // this is despite the fact that even their own type params A & B get erased before runtime
  def isSubType[A, B](implicit ttagA: TypeTag[A], ttagB: TypeTag[B]): Boolean = {
    ttagA.tpe <:< ttagB.tpe
  }

  class Animal
  class Dog extends Animal
  println(isSubType[Dog, Animal]) // prints true


  // type Tags can very sweetly be linked to the rest of the reflection API

  // 3 - get symbol
  val anotherMethodSymbol = typeTag[Person].tpe.decl(ru.TermName(methodName)).asMethod
  // 4 - reflect the method = can DO things
  val sameMethod = reflected.reflectMethod(anotherMethodSymbol)

  sameMethod.apply() // returns My name is Mary




}
