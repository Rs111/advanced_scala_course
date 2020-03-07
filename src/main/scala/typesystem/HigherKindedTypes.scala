package typesystem

object HigherKindedTypes extends App {

  // what are higher kinded types?
  // they are a deeper generic type with some unknown type parameter at the deepest level

  trait AHigherKindedType[F[_]] // this is a higher kinded type

  // what are they used for?
  // we know by now that there are a lot of Monads that have the flatMap method
  trait MyList[T] {
    def flatMap[B](f: T => B): MyList[B]
  }
  trait MyOption[T] {
    def flatMap[B](f: T => B): MyOption[B]
  }

  // we also have similar methods for all monads
  // e.g. combine/multiply List(1,2) * List(a,b) => List(1a,1b,2a,2b)

  // build for list
  def multiply[A, B](listA: List[A], listB: List[B]): List[(A,B)] = {
    for {
      a <- listA
      b <- listB
    } yield (a, b)
  }

  // same exact structure works for option
  def multiply[A, B](optionA: Option[A], optionB: Option[B]): Option[(A,B)] = {
    for {
      a <- optionA
      b <- optionB
    } yield (a, b)
  }


  /*** KEY: because we are advanced scala programmers, we want to create a common API for all of these (despite the fact that List, Option are unrelated) ***/
  // we will use higher-kinded types
  // F[_] is either list, or option, or future, ect (any monad)
  // A is the type parameter that the monad contains
  // remember: fundamental operation of monad is flatMap
  trait Monad[F[_], A] { // this is basically a higher-kinded type class
    def flatMap[B](f: A => F[B]): F[B]
    def map[B](f: A => B): F[B]
  }

  class MonadList[A](list: List[A]) extends Monad[List, A] {
    override def flatMap[B](f: A => List[B]): List[B] = list.flatMap(f) // flatMap and map are the implementations of the underlying lists
    override def map[B](f: A => B): List[B] = list.map(f)
  }

  val monadList = new MonadList(List(1,2,3))
  monadList.flatMap(x => List(x, x + 1)) // notice that from Monand[List, Int] we return List[Int] by flatMapping
  monadList.map(_ * 2) // again: a Monand[List, Int] turns into a List[Int] by mapping

  // remember F = List, or Option
  def multiply[F[_], A, B](ma: Monad[F, A], mb: Monad[F, B]): F[(A, B)] = {
    // note: for-comp only works for collections that have the map and flatmap implemented
    for {
      a <- ma
      b <- mb
    } yield (a, b)
  }

  multiply(new MonadList(List(1,2)), new MonadList(List("a", "b"))) // this compiles & runs


  // in the above, we basically created a wrapper around List
  // the same exact code would work for any monad
  // wouldn't it be nice if we could automatically convert our base (e.g. List, Option) to our monad?

  implicit class MonadList2[A](list: List[A]) extends Monad[List, A] {
    override def flatMap[B](f: A => List[B]): List[B] = list.flatMap(f) // flatMap and map are the implementations of the underlying lists
    override def map[B](f: A => B): List[B] = list.map(f)
  }

  implicit class MonadOption2[A](option: Option[A]) extends Monad[Option, A] {
    override def flatMap[B](f: A => Option[B]): Option[B] = option.flatMap(f) // flatMap and map are the implementations of the underlying lists
    override def map[B](f: A => B): Option[B] = option.map(f)
  }

  // implicit keyword makes both parameters after it implicit
  // forcing the compiler to search for implicit wrappers over a List or an Option into their Monad counterparts
  // basically our Monad acts like a type-class over our F higher kinded type
  def multiply2[F[_], A, B](implicit ma: Monad[F, A], mb: Monad[F, B]): F[(A, B)] = {
    // note: for-comp only works for collections that have the map and flatmap implemented
    for {
      a <- ma
      b <- mb
    } yield (a, b)
  }

  multiply2(List(1,2), List("a", "b")) // this compiles & runs
  multiply2(Option(2), Option("b")) // this compiles & runs
}
