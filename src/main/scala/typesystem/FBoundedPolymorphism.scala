package typesystem

object FBoundedPolymorphism extends App {

  // discuss design problem
  // If I have a class hierarchy, how do I force a method in the super type to accept a "current type"

  trait Animal {
    def breed: List[Animal]
  }

  class Cat extends Animal {
    override def breed: List[Animal] = ??? // we would like this to return List[Cat] not List[Animal]
  }

  class Dog extends Animal {
    override def breed: List[Animal] = ??? // we would like this to return List[Dog] not List[Animal]
  }

  /* question: how do I get the compiler to force the above?
    - Attempt 1 (naieve): you can just manually change it, and than works
      - works because List is covariant
   */
  // Attempt 1
  class Cat extends Animal {
    override def breed: List[Cat] = ???
  }

  class Dog extends Animal {
    override def breed: List[Dog] = ???
  }

  // the issue is that compiler also lets you do this:
  class Dog extends Animal {
    override def breed: List[Cat] = ???
  }


  // Attempt 2
  // notice how Animal appears in it's own type signature; this is called RECURSIVE TYPE
  // this particular recursive type, `A <: Animal[A]`, is called F-Bounded Polymorphism
  trait Animal[A <: Animal[A]] {
    def breed: List[Animal[A]]
  }

  class Cat extends Animal[Cat] {
    override def breed: List[Cat] = ??? // can put either Cat or Animal[Cat]
  }

  class Dog extends Animal[Dog] {
    override def breed: List[Animal[Dog]] = ???
  }

  // this is often used in database APIs; you have something called that goes into a DB defined like
  trait Entity[E <: Entity[E]] // ORM
  // another example is something like this:
  class Person extends Comparable[Person] {
    override def compareTo(o: Person): Int = ???
  }

  // this method has it's human error potential too though; the below works
  class Crocodile extends Animal[Dog] {
    override def breed: List[Animal[Dog]] = ???
  }


  // Attempt 3: how do I make the compiler enforce that the class that I am defining and the class A that I;m annotating it with are the same class
  // we can use FBounded PolyMorphism in conjunction with self types
  trait Animal[A <: Animal[A]] {self: A => // whatever descendents of Animal[A] I implement, they must also be an A
    def breed: List[Animal[A]]
  }

  class Cat extends Animal[Cat] { // i.e. now we must implement like A extends Animal[A]
    override def breed: List[Cat] = ??? // can put either Cat or Animal[Cat]
  }

  class Dog extends Animal[Dog] {
    override def breed: List[Animal[Dog]] = ???
  }

  //illegal inheritance;self-type.FBoundedPolymorphism does not conform to A (A being Dog)
  // what compiler things:  A is Dog, therefore  class extending must be A
  class Crocodile extends Animal[Dog] {
    override def breed: List[Animal[Dog]] = ???
  }

  // this also has limitations though
  // once we bring down our hierarchy another level, FBounded Polymorphism starts being shitty
  trait Fish extends Animal[Fish] // this works
  class Shark extends Fish {
    override def breed: List[Animal[Fish]] = ??? // gives us Fish, and actually won't let us do shark
  }

  class Code extends Fish {
    override def breed: List[Animal[Fish]] = List(new Shark) // this works too, which is wrong because Sharks should not breed to Cods
  }


  // Attempt 4
  // can try to use Type Classes to get around the problem above
  // this works well but is kind of weird/complicated

  trait Animal
  trait CanBreed[A] {
    def breed(a: A): List[A] // basically forcing that Cat must breed to a list of Cats and dogs must breed to list of dogs
  }
  class Dog extends Animal
  object Dog {
    implicit object DogsCanBreed extends CanBreed[Dog] {
      override def breed(a: Dog): List[Dog] = ???
    }
  }

  implicit class CanBreedOps[A](animal: A) {
    def breed(implicit canBreed: CanBreed[A]): List[A] = canBreed.breed(animal)
  }


  val dog = new Dog
  dog.breed // compiler does new CanBreedOps[Dog](dog).breed(Dof.DogsCanBreed(dog))

  // lets try to write some bad code
  class Cat extends Animal
  object Cat {
    implicit object CatsCanBreed extends CanBreed[Dog] { // wrote this wrong
      override def breed(a: Dog): List[Dog] = ??? // wrote this wrong
    }
  }

  val cat = new Cat
  // get a compilation error here
  // can not find implicit parameter for canBreed that returns a Cat
  cat.breed


  // Attempt 5
  // let's consider if trait Animal was itself the type class
  trait Animal[A] {
    def breed(a: A): List[A]
  }

  class Dog
  object Dog {
    implicit object DogAnimal extends Animal[Dog] {
      override def breed(a: Dog): List[Dog] = List()
    }
  }

  implicit class AnimalOps[A](animal: A) {
    def breed(implicit animalTypeClassInstance: Animal[A]): List[A] = {
      animalTypeClassInstance.breed(animal)
    }
  }

  val dog = new Dog
  dog.breed




























}
