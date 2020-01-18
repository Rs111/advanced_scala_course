

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
}
