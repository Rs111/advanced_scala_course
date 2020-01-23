

object ThreadsIntro extends App {

  // this lesson will focus on creation, manipulation, and communication of JVM threads
  // one of the critical pieces of parallel programming on JVM is called a thread
  /*Thread: is the instance of a class, like everything else on JVM
  - Thread takes an instance of a trait called `Runnable` as a Java interface
    - Scala trait ~= Java Interface
  - we will treat java interfaces as traits in scala
  */
  /*
  in the package java.lan, there is an interface called Runnable, and has a method called run which doesn't take params

  interface Runnable {
    public void run()
  }
   */

  // this piece of code instantiates a Thread object with a runnable object whose method run prints something in parallel
  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("running in parallel")
  })

  // how do you run something in parallel: this line of code
  // create a JVM thread which runs on top of an OS thread
  // when you run this code, the running in parallel code will print, and it will have been executed on top of a seperate JVM thread than the Thread that evaluated every other piece of code
  // this gives the signal to the JVM to start a new JVM thread, which runs on top of an OS thread
  // then the JVM makes that JVM thread invoke the run method inside its inner runnable
  // Note: do not confuse our Thread instance with the JVM thread instance where our code is actually run
  // Note: you call the start method on the thread, NOT the run method on the runnable
  aThread.start()

  // blocks until aThread has finishes running
  // this is how you make sure a Thread has already run before continuing some computation
  aThread.join()

  // you'll see goodbye/hello/goodbye/hello/hello/goodbye/goodbye
  // if you run it again, you'll get a different order
  // why? Different runs in a multithreaded enviornment produce different results
  // thread scheduling depends on a number of factors, incl. operating system and JVM implementation
  val threadHello = new Thread(() => (1 to 5).foreach(_ => println("hello")))
  val threadGoodbye = new Thread(() => (1 to 5).foreach(_ => println("goodbye")))

  /*
    Threads are really expensive to start and kill
    solution: re-use them
    can re-use threads with executors
    note: there are more types of executors than just FixedThreadPool
   */
  import java.util.concurrent.Executors
  val pool = Executors.newFixedThreadPool(10) // pass in number of threads you want to re-use
  // will get executed by one of the 10 threads managed by executors
  // I now don't need to care about starting and stopping threads, which is good
  pool.execute(() => println("something in the thread pool"))
  pool.execute( () => {
    Thread.sleep(1000)
    println(" done after 1 sec")
  })
  pool.execute( () => {
    Thread.sleep(1000)
    println("almost done")
    Thread.sleep(1000)
    println("done after 2 seconds")
  })

  // shut down pool
  pool.shutdown() // no more actions can be submitted
  pool.execute(() => println("should not appear")) // throws exception in main thread (calling thread)

  // this command is different; interrupts the sleeping Threads above that are currently under the pool
  // if they are sleeping, they will throw exceptions
  // if you call pool.shutdownNow() in the main thread while the actions above are executing, the actions will throw exceptions
  pool.shutdownNow()
  // returns true after the shutdownNow command is called sometime in the past
  // this one is tricky though: it runs before everything else...
  // ...if you comment out the shutDownNow and run isShutdown, it will return if the pool had previously been shut down
  pool.isShutdown


  /* let's illustrate pain point where we write to x from two different threads */
  def runInParallel: Unit = {
    var x = 0

    val thread1 = new Thread(() => {
      x = 1
    })
    val thread2 = new Thread(() => {
      x = 2
    })

    thread1.start()
    thread2.start()
    println(x)
  }

  // in most cases we get 0, which means println executed before the threads actually ran (but this is not always the case)
  // so...not ideal
  // this is called a `race condition`: two threads are attempting to reset the same memory zone at the same time
  for (_ <- 1 to 100) runInParallel

  // another example of race condition
  class BankAccount(var amount: Int) {
    override def toString: String = "" + amount
  }

  def buy(account: BankAccount, thing: String, price: Int): Unit = {
    account.amount -= price
    println("I've bought " + thing)
    println("my account is now " + account)
  }


  // we had one record with 46k, which means user bought both shoes and iphone
  // how could this happen? both threads started with 50k, thread1 finishes first, then thread2
  for (_ <- 1 to 10000) {
    val account = new BankAccount(50000)
    val thread1 = new Thread(() => buy(account, "shoes", 3000))
    val thread2 = new Thread(() => buy(account, "iPhone", 4000))

    thread1.start()
    thread2.start()
    Thread.sleep(10)
    if (account.amount != 43000) println("aha: " + account.amount)
  }

  // Java & JVM have tools for us to battle race conditions
  // option one is more powerful and more used; allows you to put more stuff in same synchronized block
  /* #option #1: use synchronized()
    - is a method on reference types
    - if you do same test above with buySafe, not going to see any more weirdness
   */
  def buySafe(account: BankAccount, thing: String, price: Int): Unit = {
    // takes type param T and a value of type T
    account.synchronized {
      // no two threads can evaluate the parameter that I'm passing to synchronized at the same time
      account.amount -= price
      println("I've bought " + thing)
      println("my account is now " + account)
    }
  }

  /* #option #2: use @volatile
    - annotation for a var: means that all reads and writes to it are synchronized
   */
  class BankAccount2(@volatile var amount: Int) {
    override def toString: String = "" + amount
  }


  // construct inception threads (1-50); thread1 -> thread2 -> thread3 -> ...
  // print(hello from thread x) in reverse order
  def inceptionThreads(maxThreads: Int, i: Int = 1): Thread = {
    new Thread(() => {
      if (i < maxThreads) {
        val newThread = inceptionThreads(maxThreads, i + 1)
        newThread.start() // start it
        newThread.join() // wait for it to finish
      }
      println(s"hello from thread $i")
    })
  }

  inceptionThreads(50).start()


  // what is the biggest possible value for x? what is the smallest possible?
  // answers: 100 (all threads can act sequentially), 1 (for 1, all threads can act in parallel)
  var x = 0
  val threads = (1 to 100).map(_ => new Thread(() => x += 1))
  threads.foreach(_.start())


  // sleep fallacy; whats the value of message? is it guaranteed?
  // NOTE: sleep yields CPU for AT LEAST that many milliseconds
  // this is a bad practice (synchronizing threads by putting them to sleep at different times)
  // if you put a thread to sleep for a sec, and another for 2 sec, it does not mean that the operations would be executed in that order
  // value of messages will almost always be 'scala is awesome', but it is not guaranteed
  /* execution might go like this:

  (main thread)
    message = "Scala sucks"
    awesomeThread.start()
    sleep(2000) - relieves execution, frees CPU to execute some other thread at the discretion of the OS
  (awesome thread)
    gets the CPU
    sleep(1000) - also relieves execution
    at this point, the OS is free to choose some running thread on the OS
  (OS gives the CPU to some important thread, which takes CPU more than 2 seconds)
    after important thread has finished running, the OS gives back the CPU to our program
    at this point, both the main thread and the awesome thread have finished sleeping
    OS gives CPU back to main thread, not to awesome thread
    println executes
  (OS gives cpu to awesome thread)
    message = scala is awesome (but by this
  */
  var message = ""
  val awesomeThread = new Thread(() => {
    Thread.sleep(1000)
    message = "Scala is awesome"
  })

  message = "Scala sucks"
  awesomeThread.start()
  Thread.sleep(2000)
  println(message)


  // how do we fix the above?
  // synchronizing doesn't work here: synchronizing is only useful for concurrent modifications
  // i.e. if two threads are attempting to modify message at the same time, then sync can work
  // here we have a sequential problem; the only solution is to have threads join

//  var message = ""
//  val awesomeThread = new Thread(() => {
//    Thread.sleep(1000)
//    message = "Scala is awesome"
//  })
//
//  message = "Scala sucks"
//  awesomeThread.start()
//  Thread.sleep(2000)
  //awesomeThread.join() // wait for awesomeThread to join
//  println(message)







}
