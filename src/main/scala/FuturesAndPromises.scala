object FuturesAndPromises extends App {

  /*
    Futures
    - a functional way of computing something in parallel (or on another thread)
    - a future is a computation that will hold a value which is computed by somebody (some thread) at some point in time

   */

  // lets say this calculates a very long computation
  def calculateMeaingOfLife: Int = {
    Thread.sleep(2000)
    42
  }

  import scala.concurrent.Future
  // compiler looks for an implicit execution context value to plug in as implicit param into Future
  // execution context is similar to an executor; it holds a thread pool equal to the number of processors
  // execution context basically handles thread allocation to Futures
  import scala.concurrent.ExecutionContext.Implicits.global
  val aFuture = Future {
    calculateMeaingOfLife // calculate meaning of life on ANOTHER thread
  } // global which is passed by the compiler

  // returns Option[Try[Int]] = None
  // computation might have failed with an exception (Try) or it might not have finished by time you call print (Option)
  println(aFuture.value)

  println("Waiting on the future")
  // takes f: Try[Int] => NotInferedU as an arguement because when the future completes it will complete with a Try[Int]
  import util.{Try, Success, Failure}
  // is a partial function which operates on each element of aFuture
  /*
    Notes on onComplete
      - returns unit so it's used for side effects
      - the onComplete block/function is called a `callback`
      - callback will be called by `some` thread; it may be this thread that created the callback, it may be the thread that ran the future, it may be some other thread
      - don't make any assumptions about which thread runs the callback
   */
  aFuture.onComplete{
    case Success(meaningOfLife) => println("success")
    case Failure(e) => println(s"I have failed with $e")
  }


  // build a mini social network; this is huge, so things need to happen asynchronously
  case class Profile(id: String, name: String) {
    def poke(anotherProfile: Profile): Unit = {
      print(s"${this.name} poking ${anotherProfile.name}")
    }
  }

  object SocialNetwork {
    // "database"
    val names = Map(
      "fb.id.1-zuck" -> "Mark",
      "fb.id.2-bill" -> "Bill",
      "fb.id.0-dummy" -> "Dummy"
    )

    val friends = Map(
      "fb.id.1-zuck" -> "fb.id.2-bill"
    )

    // social network has computation when it fetches a profile; simulate with sleep
    import scala.util.Random
    val random = new Random()

    // Our social network has an API consisting of two methods
    // return a future that will hold a profile at some point
    def fetchProfile(id: String): Future[Profile] = Future {
      // simulates fetch from database
      Thread.sleep(random.nextInt(300))
      Profile(id, names(id))
    }

    def fetchBestFriend(profile: Profile): Future[Profile] = Future {
      Thread.sleep(random.nextInt(400))
      val bfId = friends(profile.id)
      Profile(bfId, names(bfId))
    }
  }

  // let's assume that in client app, we want mark to poke bill
  // how would we do that if the client called an endpoint in our app?
  // we would create a Future by fetching zuck's profile, then fetching bill's profile by zuck's best friend, then call poke
  // code works but it's ugly/nested
  val mark = SocialNetwork.fetchProfile("fb.id.1-zuck")
  mark.onComplete {
    case Success(markProfile) => {
      val bill = SocialNetwork.fetchBestFriend(markProfile)
      bill.onComplete {
        case Success(billProfile) => markProfile.poke(billProfile)
        case Failure(e) => e.printStackTrace()
      }
    }
    case Failure(e) => e.printStackTrace()
  }

  // make sure Futures from social network have enough time to finish
  Thread.sleep(1000)


  // better way to fix the above: funcitional composition of futures
  // map, flatMap, filter

  // Future[Profile] => Future[String]
  // if the original future fails with an exception, then the mapped future will fail with the same exception
  val nameOnTheWall = mark.map(profile => profile.name)

  val marksBestFriend = mark.flatMap(profile => SocialNetwork.fetchBestFriend(profile))

  // can filter a future with a predicate; it will return future of the same type
  // or if predicate fails, this future will fail with noSuchElement Exception
  val zucksBestFriendRestricted = marksBestFriend.filter(profile => profile.name.startsWith("Z"))

  // for comprehensions
  // given marks profile obtained after completing this future
  // given bills profile obtained after completing this future
  // do this thing
  for {
    mark <- SocialNetwork.fetchProfile("fb.id.1-zuck")
    bill <- SocialNetwork.fetchBestFriend(mark)
  } mark.poke(bill)

  /** fallbacks in case things go wrong**/
  // one fallback path is called recovery
  // fetch profile relies on that id being there in the database, so if id is not known, this will throw an exception within the future
  // we would like to recover our future with a dummy profile in case there is an exception or throwable
  // in case future fails with exception inside, we can recover it
  // recover takes partial function and returns `U` (e.g. Profile)
  val aProfileNoMatterWhat = SocialNetwork.fetchProfile("unknown").recover {
    case e: Throwable => Profile("fb.id.0-dummy", "dummy name")
  }

  // in the case when we don't want to return a profile itself but to fetch another profile from social network
  // we can use recoverWith; takes partial function and returns `Future[U]`, e.g. Future[Profile]
  val aFetchedProfileNoMatterWhat = SocialNetwork.fetchProfile("unknown").recoverWith {
    case e: Throwable => SocialNetwork.fetchProfile("fb.id.0-dummy")
  }

  // same result as the recoverWith
  /* creates new future based on the following logic:
      - if original future succeeds, it's value will be used for the resulting future
      - if it fails with an exception then the arguement in fallback will be run, and if that succeeds that value will be used
      - if that also fails, the exception of the first future will be contained in the resulting future
  */
  val fallBackResult = SocialNetwork.fetchProfile("unknown").fallbackTo(SocialNetwork.fetchProfile("fb.id.0-dummy"))



  /**** sometimes we want to block on a Future ****/
  //NOTE: on complete is for unit, while await is for value/expressions
  // e.g. bank transactions; you want to make sure an operation is fully complete before you move on to display results or do something else

  case class User(name: String)
  case class Transaction(sender: String, receiver: String, amount: Double, status: String)

  object BankingApp {
    val name = "Rock the JVM banking"

    def fetchUser(name: String): Future[User] = Future {
      // simulate fetch from db
      Thread.sleep(500)
      User(name)
    }

    def createTransaction(user: User, merchantName: String, amount: Double): Future[Transaction] = Future {
      // simulate long computation
      Thread.sleep(1000)
      Transaction(user.name, merchantName, amount, "SUCCESS")
    }

    import scala.concurrent.Await
    import scala.concurrent.duration._
    def purchase(username: String, item: String, merchantName: String, cost: Double): String = {
      // fetch user from db
      // create transaction from username to merchant name
      // WAIT for transaction to finish
      val transactionStatusFuture = for {
        user <- fetchUser(username)
        transaction <- createTransaction(user, merchantName, cost)
      } yield transaction.status

      // if duration has passed, this will throw exception with a timeout
      Await.result(transactionStatusFuture, 2.seconds) // this is how we block until completetion of this function
      // the seconds is done via implicits
    }

    def purchase2(username: String, item: String, merchantName: String, cost: Double): Future[String] = {

      for {
        user <- fetchUser(username)
        transaction <- createTransaction(user, merchantName, cost)
      } yield transaction.status
    }
  }

  var x = 5
  val radu = User("radu")
  // this blocks until all those futures involved are complete
  println(BankingApp.purchase("radu", "pizza", "dominos", 10.0))

  BankingApp.purchase2("radu", "pizza", "dominos", 10.0).onComplete{
    case Success(s) => x = 3
    case Failure(s) => x = 2
  }





  /***** Manual manipulation of Futures with Promises *****/
  // notice that up until now we can only read or manipulate the results from futures either by calling onComplete, for{}, or Await.result
  // in a sense, Futures are READ-ONLY when they are done
  // but sometimes we need to complete or set a Future at a point of our choosing

  // promises
  // think of promises as a "controller" over a future
  // promise has a member called future which it holds and manages
  import scala.concurrent.Promise
  val promise = Promise[Int]()
  val future = promise.future // future is under the hold/management of the promise

  // small producer/consumer impl using futures & promises
  // thread 1 - "consumer"; consumer knows how to handle future's completion
  // "promise pattern": one thread knows how to handle a Future and one thread inserts values or failures into the future by calling promise.success(..) or .failure(e)
  // gives you the power to set a value to a future when and how you see fit
  future.onComplete {
    case Success(r) => println("[consumer] I've received " + r)
  }

  val producer = new Thread(() => {
    println("[producer] crunching numbers...")
    Thread.sleep(1000)
    // "fulfilling" the promise
    // this part basically manipulates the internal future to complete with a successful value 42
    // this is then handled in onComplete by some consumer thread
    promise.success(42)
    println("[producer] done")
  })

  /*
    1) Fullfill a future IMMEDIATELY with a value
    2) inSequence(fa, fb); function takes two futures, runs fa and runs fb after it is sure fa has completed
    3) first(fa, fb) => new Future(); holds value of a if fa finishes first or b otherwise
    4) last; same idea as the question above
    5) run an action repeatedly until a condition is met; return first value that satisfies the condition
      - retryUntil(action: () => Future[T], condition: T => Boolean): Future[T]
   */

  // 1 - fullfill immediately
  def fulfillImmediately[T](value: T): Future[T] = Future(value)

  // 2 - inSequence; run second only after first has completed
  def inSequence[A, B](first: Future[A], second: Future[B]): Future[B] = {
    first.flatMap(_ => second) // once first finishes, run the second
  }

  // 3 - first out of two futures
  def first[A](fa: Future[A], fb: Future[A]): Future[A] = {
    val promise = Promise[A] // this is the controller of a future of Type A

    /* the tricky part here is:
      - let's say fa finishes first and fulfills the promise
      - before function can return, fb finishes it and tries to fullfill the promise
      - because the promise has already been fullfilled, this will throw an exception
      - need to add try/catch logic here
    */
    def tryComplete(promise: Promise[A], result: Try[A]): Unit = result match {
      case Success(r) => try {
        promise.success(r)
      } catch {
        case _ =>
      }
      case Failure(t) => try {
        promise.failure(t)
      } catch {
        case _ =>
      }
    }

    // but WAIT there is already a tryComplete method on promise; so the commented out code is actually equivalent to what we wrote
    // fa.onComplete(promise.tryComplete(_))  or fa.onComplete(promise.tryComplete); method lifted into a function value
    // tryComplete also returns a boolean if the promise could be completed by this result or not
    // it would be like if we returns true/false/boolean on our method

    // the first future to complete will fulfill the promise (i.e. call success or failure)
    // the second will fail and not to do anything
    // the returned future in the promise will therefore contain the first finished thing
    fa.onComplete(tryComplete(promise, _))
    fb.onComplete(tryComplete(promise, _))

    promise.future
  }


  // 4 - last out of two futyures
  def last[A](fa: Future[A], fb: Future[A]): Future[A] = {
    // logic: use two promises
    // create one promise which both futures will try to fullfill; the first future to complete will succeed in fulfilling this promise
    // the second future will FAIL to fulfill the first promise; this one should then be made to fullfill the second promise
    // second promise's future will be output
    val bothPromise = Promise[A]
    val lastPromise = Promise[A]
    val checkAndComplete = (result: Try[A]) => {
      // careful, calling tryComplete inside the if still produces the side effect
      // if this future was un-able to fullfill bothPromise with it's result, fullfill lastPromise
      // note: when `bothPromise.tryComplete(result)` is called, it either fulfills the promise side-effect & returns true OR it has no side-effect and returns false
      if(!bothPromise.tryComplete(result))
        lastPromise.complete(result)
    }

    fa.onComplete(checkAndComplete)
    fb.onComplete(checkAndComplete)

    lastPromise.future
  }

  val fast = Future {
    Thread.sleep(100)
    42
  }
  val slow = Future {
    Thread.sleep(2000)
    84
  }

  first(fast, slow).foreach(println)
  last(fast, slow).foreach(println)


  // 5 - retry until
  // NOTE: Future(17).filter(_ == 18) returns: Future(Failure(java.util.NoSuchElementException: Future.filter predicate is not satisfied))
  // NOTE: Future(18).filter(_ == 18) returns: Future(Success(18))
  def retryUntil[A](action: () => Future[A], condition: A => Boolean): Future[A] = {
    // trigger the action, which will return a Future
    action()
      // when Future is filtered:
      // returns a Future[A] which will either pass with a success with the value returned by the action
      // or it will fail with a noSuchElement exception if the condition does not hold for action
      .filter(condition)
      // recoverWith does nothing when called on a Future(Success(..)))
      // recoverWith returns a Future if it is called on a Future(Failure(..))
      .recoverWith {
        case _ => retryUntil(action, condition)
      }
  }

  import scala.util.Random
  val random = new Random()
  val action = () => Future {
    Thread.sleep(100)
    val nextValue = random.nextInt(100)
    println("generated " + nextValue)
    nextValue
  }

  retryUntil(action, (x: Int) => x < 10).foreach(result => println("settled at " + result))
























}
