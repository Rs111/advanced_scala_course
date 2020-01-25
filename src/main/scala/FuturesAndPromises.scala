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

}
