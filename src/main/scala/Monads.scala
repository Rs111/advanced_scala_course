object Monads extends App {

  /* - Monads are an abstract type; they are a kind of types that have some fundamental operations
     - Option, Try, Future, Stream, Set are all Monads
     - The two fundamental operations of a monad are:
        - unit: also called `pure` or `apply`: constructs a Monad out of a value
        - flatMap: also called `bind`; transforms a monad of a type parameter into a monad of another type parameter
     - Operations must satisfy MONAD LAWS
        - left-identity: if you build a basic monad out of an element and you flatMap it with a function, it should give you the function applied to that element
           - e.g. if unit is my Monad, unit(x).flatMap(f) == f(x)
        - right-identity: if you have a monad instance and you flatMap it with the unit function, then it should give you the same monad
           - aMonadInstance.flatMap(unit) == aMonadInstance
        - associativity: if you have a monad instance and you flatMap it with two functions in sequence, then it should give you same result as flatMaping with a composite function
           - m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))

     Option, Try, Future, Stream, Set are all Monads

      trait MonadTemplate[A] {
        def unit(value: A): MonadTemplate[A]
        def flatMap[B](f: A => MonadTemplate[B]): MonadTemplate[B]
      }

      -Picture a List; let's see how these laws apply for List:
      - key is that f: x => List(x)
      - The below shows that list is a Monad
         - List(x).flatMap(f)
         = f(x) ++ Nil.flatMap(f)
         = f(x); this satisfies left-identity law
         - list.flatMap(x => List(x))
         = list
         - let's break last one with an example:
            - [a b c].flatMap(f).flatMap(g)
            = [f(a) ++ f(b) ++ f(c)].flatMap(g)
            = [g(f(a)) ++ g(f(b)) ++ g(f(c))]
            = [f(a).flatMap(g) ++ f(b).flatMap(g) ++ f(c).flatMap(g)]
            = [a b c].flatMap(x => f(x).flatMap(g))

      - Same exercise with Option (will focus on Some case because None is trivial)
      - Option(x).flatMap(f)
        = Some(x).flatMap(f) = f(x)
      - opt.flatMap(x => Option(x))
        = Some(v).flatMap(x => Option(x))
        = Some(x)
      - o.flatMap(f).flatMap(g)
        - o.flatMap(f).flatMap(g) = f(v).flatMap(g)  // based on first identity
        - o.flatMap(x => f(x).flatMap(g)) = f(v).flatMap(g) // based on second
        = also
        = Some(f(o)).flatMap(g)
        = Some(g(f(o)))

   */


  // creating our Try Monad
  trait Attempt[+A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B]
  }

  object Attempt {
    // using call-by-name because we don't want it to be evaluated when we build the attempt
    // the evaluation of the parameter might throw exceptions and we want to prevent that
    def apply[A](a: => A): Attempt[A] =
      try {
        Success(a)
      } catch {
        case e: Throwable => Fail(e)
      }
  }

  case class Success[+A](value: A) extends Attempt[A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B] =
      try {
        f(value)
      } catch {
        case e: Throwable => Fail(e)
      }
  }

  case class Fail(e: Throwable) extends Attempt[Nothing] {
    def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] = this
  }

  /**
    * ---prove monad laws
    *
    * left-identity
    *
    * unit.flatMap(f) = f(x)
    * Attempt(x).flatMap(f) = f(x) // only makes sense for the Success Case!
    * Success(value).flatMap(f) = f(value)
    *
    * right-identity
    * attempt.flatMap(unit) = attempt
    * Success(x).flatMap(x => Attempt(x)) = Success(x)
    * Fail(_).flatMap(..) = Fail(e) // works
    *
    * associativity
    * attempt.flatMap(f).flatMap(g) = attempt.flatMap(x => f(x)).flatMap(g))
    *
    * Fail(e).flatMap(f).flatMap(g) = Fail(e)
    * Fail(e).flatMap(x => f(x).flatMap(g)) = Fail(e)
    *
    * Success(x).flatMap(f).flatMap(g) = f(x).flatMap(g)
    * Success(x).flatMap(x => f(x).flatMap(g)) = f(x).flatMap(g)
    */


  val attempt = Attempt {
    throw new RuntimeException("My own monad")
  }

  println(attempt)

  /* Lazy[T] monad
   abstracts away a computation that will only be evaluated when it's needed
  implement unit/apply
  flatMap
  */

  // my work
  trait Lazy[+A] {
    def flatMap[B](f: (=> A) => Lazy[B]): Lazy[B]
    def get: A
  }

  object Lazy {
    def apply[A](value: => A): Lazy[A] = new NonEmptyLazy(value)
  }

  object EmptyLazy extends Lazy[Nothing] {
    def flatMap[B](f: (=> Nothing) => Lazy[B]): Lazy[B] = this
    def get: Nothing = throw new NoSuchElementException
  }

  class NonEmptyLazy[+A](value: => A) extends Lazy[A] {
    def flatMap[B](f: (=> A) => Lazy[B]): Lazy[B] = f(value)
    lazy val get: A = value // call by need
  }

  val x = Lazy(4 + 4).flatMap(x => Lazy(x + 5)).get

  // profs work; mine was correct too
  class Laz[+A](value: => A) {
    private lazy val internalValue = value
    def use: A = internalValue
    def flatMap[B](f: (=> A) => Laz[B]): Laz[B] = f(value)
  }

  object Laz {
    def apply[A](value: => A): Laz[A] = new Laz(value)
  }

  /*
  - We saw definition of monads as something that defines unit + flatMap, along with monad laws
  - online you may see another definition: Monad = unit + map + flatten
  - map and flatten can be implemented in terms of flatMap
  - given
      Monad[T] { // think of this as list
        def flatMap[B](f: T => Monad[B] = ... implemented

        // create the two below
        def map[B](f: T => B): Monad[B] = flatMap(x => unit(f(x)))
        def flatten(m: Monad[Monad[T]]): Monad[T] = m.flatMap(x: Monad[T] => x)

        map: List(1,2,3).map(_ * 2) = List(1,2,3).flatMap(x => List(x * 2))
        flatten: List(List(1), List(2), List(3)).flatten = List(List(1), List(2), List(3)).flatMap(x => x)
      }
   */



















}
