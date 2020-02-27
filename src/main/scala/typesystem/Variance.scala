package typesystem

object Variance extends App {

  trait Animal
  class Dog extends Animal
  class Cat extends Animal
  class Crocodile extends Animal

  /***variance ***/
  // variance is the problem of: "inheritance" - type substitution of generics
  /*
    e.g. should a Cage[Cat] inherit from Cage[Animal] ?
    Yes - covariance
    No - invariance
    Hell No (opposite) - contravariant
   */
  class Cage[T]

  // covariant: the line below works because Cat extends Animal and CCage[Cat] extends CCage[Animal]
  // same idea as us being able to say: val x: Any = "s"
  // makes sense intuitively: a cat cage is a kind/subset of an animal cage
  class CCage[+T]
  val ccage: CCage[Animal] = new CCage[Cat]

  // invariant
  // it's as if you said -> val x: Int = "hello world"
  class CCageInv[T]
  val ccageInv: CCageInv[Animal] = new CCage[Cat] // compiler complains

  // contravariant
  // why on earth would we want this?
  // counter intuitive, but: I am replacing a specific cage of cats with a general cage of animals
  // if RS can contain an animal, it can also contain a cat
  class CCageCont[-T]
  val ccage: CCageCont[Cat] = new CCageCont[Animal]

  class InvariantCage[T](animal: T) // invariant

  /***covariant and contravariant positions ***/
  // are basically some compiler restrictions to avoid weird things (as shown below)

  // covariant positions
  //we say that the generic type of vals/fields are in a covariant position
  // that means in constructor (e.g. val animal...) the compilter accepts fields with covariant types
  // covariant positions also accept invariant types
  // E.g. invariant T and +T are both accepted in Invariantcage and CovariantCage respectively
  // what doesn't work: Constravariant
  class CovariantCage[+T](val animal: T)
  class ContravariantCage[-T](val animal: T) // this will fail if compiler; contravariant type occurs in covaraint position

  /*
    the reason compiler makes that code fail is to avoid something like:
      - a Crocodile is an animal, so you can put crocodile as a param
      - but the problem is that what I wanted was a more specific cage, and you're filling it with some other kind of animal
      -
    val catCage: ContraCage[Cat] = new ContraCage[Animal](new Crocodile)
   */

  /*
    similar thing occurs for var fields
   */
class ContravariantVariableCage[+T](var animal: T) //fails: covariant type T occurs in contravariant position in type T
  /*
    if the compiler was able to pass that code, I would be able to write:

    val ccage: CovariantCage[Animal] = new CovariantCage[Cat](new Cat)
    ccage.animal = new Crocodile // this is a problem because your instantiation is a cat
   */

  class ContravariantCage[-T](var animal: T) // also doesn't work for the same reason as vals

  // The only acceptable type for a variable field is invariant
  class InvariantvariantCage[T](var animal: T)


  trait AnotherCovariantCage[+T] {
    def addAnimal(animal: T) // method arguements are CONTRAVARIANT POSITION
  }
  /*
    does not compile because:
    EG 1
    val ccage: CovariantCage[Animal] = new CovariantCage[Dog]
    ccage.add(new Cat) // we don't want Cats and Dogs in the same place

    EG 2
    -- Cat extends Animal, therefore Printer[Animal] extends Printrt[Cat]
    -- Animal printer should know how to print any Animal, including any Cat (which is a subtype of animal
    -- the reverse is not true
    -- i.e. covariance wouldn't work, because we would be saying that a Printer[Cat] should know how to print any animal, which is not true

    abstract class Printer[-A] {
      def print(value: A): Unit
    }

    class AnimalPrinter extends Printer[Animal] {
      def print(animal: Animal): Unit =
        println("The animal's name is: " + animal.name)
    }

    class CatPrinter extends Printer[Cat] {
      def print(cat: Cat): Unit =
        println("The cat's name is: " + cat.name)
    }

    object ContravarianceTest extends App {
      val myCat: Cat = Cat("Boots")

      def printMyCat(printer: Printer[Cat]): Unit = {
        printer.print(myCat)
      }

      val catPrinter: Printer[Cat] = new CatPrinter
      val animalPrinter: Printer[Animal] = new AnimalPrinter

      printMyCat(catPrinter)
      printMyCat(animalPrinter)
    }
   */

  // this works
  // we have a cage of animal that we want to make more specific; now have something
  // addAnimal should be able to do something on "any" Cat or subtype; but if animal is Dog or Animal, we might not
  class AnotherContravariantCage[-T] {
    def addAnimal(animal: T) = true
  }
  val acc: AnotherContravariantCage[Cat] = new AnotherContravariantCage[Animal]
  acc.addAnimal(new Dog) // variance expected: Cat, but actual: Dog; must receive arguement of Cat or below
  acc.addAnimal(new Animal) // does not work
  acc.addAnimal(new Cat) // works
  class Kitty extends Cat //works
  acc.addAnimal(new Kitty) //works

  // the above is painful because we want to be able to have covariant lists that ww can add to
  // solution
  class MyList[+A] {
    def add[B >: A](element: B): MyList[B] = new MyList[B] //we "widen" the type
  }

  val emptyList = new MyList[Kitty]
  val animals = emptyList.add(new Kitty) // kitty ia either a type kitty or a super type
  val moreAnimals = animals.add(new Cat) // this is the situation that the compiler tries to avoid, but in this case it is happy (returns List[Cats])
  val evenMoreAnimals = moreAnimals.add(new Dog) // this works too, because of B >: A; compiler


  /* Idea
      - we want to maintain that every element in our collection has the same type
      - if something is a List[Cat], it can only contain Cats
      - when we add an Animal, this will break (which forces compiler to widen type)
   */


  /*** methods return types ***/
  class PetShop[-T] {
    def get(isItAPuppy: Boolean): T //compiler doesn't like this; Method Return Types are covariant position!
    def get[S <: T](isItAPuppy: Boolean, defaultAnimal: S): S // added later
  }
  /*
    if compiler passed this code, I would be able to write something like

    val catShop = new PetShop[Animal] {
      def get(isItAPuppy: Boolean): Animal = new Cat
    }

    // you would be able to do this because PetShop[Anima] would extend Petshop[Dog]
    val dogShop: PetShop[Dog] = catshop // so you would have a cat in the api of a dog
   */

  // way to solve this would be the opposite of the list: i.e. def method[B <: A](x: Any): B
  def get[S <: T](isItAPuppy: Boolean): S

  val shop: PetShop[Dog] = new PetShop[Animal]
  val evilCat = shop.get(true, new Cat) // doesn't work
  val TerraNova extends Dog
  val bigFurry = shop.get(true, new TerraNova)


  /*
    Big Rule
    - method arguements are in CONTRAVARIANT positions
    - Return Types are in COVARIANT positions

    Rule of Thumb
    - use convariance if you have a COLLECTION OF THINGS
      - if using parking as collection of vehicles, use covariance
    - if you want to use parking as collection of ACTIONS you want to perform on types, use contravariance
      - i.e. group of ACTIONS
    - e.g. in our example, contravariant is probably the best because it's a bunch of actions we want to apply to vehicles
   */

  /*
    Excercises
    - Backend API designer for a parking application which checks for illegal parking
    - Design Invariant, covariant, contravariant versions of:
                                                  Parking[T](things: List[T]) {
                                                    def park(vehicle: T)
                                                    def impound(vehicles: List[T])
                                                    def checkVehicles(conditions: String): List[T]
                                                  }
    - things are some sort of vehicles; cars, bikes
   */

  class Vehicle
  class Bike extends Vehicle
  class Car extends Vehicle
  class IList[T]

  // boring; allow containment of only one type
  class IParking[T](vehicles: List[T]) {
    def park(vehicle: T): IParking[T] = ???
    def impound(vehicles: List[T]): IParking[T] = ???
    def checkVehicles(conditions: String): List[T] = ???

    def flatMap[S](f: T => IParking[S]): IParking[S] = ???
  }

  class CParking[+T](vehicles: List[T]) {
    def park[S >: T](vehicle: S): CParking[S] = ??? // parking a new vehicle turns parking of a given type into a wider type
    def impound[S >: T](vehicles: List[S]): CParking[S] = ??? // in practice, we would never widen here in RL (can't widen by removing)
    def checkVehicles(conditions: String): List[T] = ???

    def flatMap[S](f: T => CParking[S]): CParking[S] = ???
  }

  class XParking[-T](vehicles: List[T]) {
    def park(vehicle: T): XParking[T] = ???
    def impound(vehicles: List[T]): XParking[T] = ???
    def checkVehicles[S <: T](conditions: String): List[S] = ???

    // original doesn't work: def flatMap[S](f: T => CParking[S]): CParking[S] = ???
    // f = Function1[T, XParking[S]
    // we Function1 T is contravariant in T; i.e. it is in an inverse variance relationship to T
    // that makes our T in the function below a double contravariant position, which is covariant
    //
    def flatMap[R <: T, S](f: R => XParking[S]): XParking[S] = ???
  }


}
