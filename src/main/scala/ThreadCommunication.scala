object ThreadCommunication extends App {

  // with the tools we know so far, we cannot really enforce a certain order of exec. on threads
  // this lesson will fix that
  /*
    we will work on classic problem: the producer-consumer problem
      - start with small container that wraps a single value
      - in parallel, we have two threads running:
        - one is called a producer: has the sole purpose of setting a value inside a container
        - second is called consumer: sole purpose is to extract the value from the container
      - problem: producer and consumer are running in parallel, at the same time, so they don't know when eachother have finished working
      - we have to somehow force consumer to wait until producer is done
      - we are going to work on different versions of this problem

      producer -> [ x ] -> consumer
   */

  class SimpleContainer {
    private var value: Int = 0

    def isEmpty: Boolean = value == 0
    // consuming method: getting item from container resets it to default value
    def get: Int = {
      val result = value
      value = 0
      result
    }
    def set(newValue: Int): Unit = value = newValue
  }

  def naiveProdCons(): Unit = {
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[consumer] waiting")
      while(container.isEmpty) {
        println("[consumer] actively waiting...") // while loop breaks
      }

      println("[consumer] I have consumed " + container.get) // extract value and reset to 0
    })

    val producer = new Thread(() => {
      println("[producer] computing ...")
      Thread.sleep(500) // simulate calc
      val value = 42
      println("[producer] I have produced, after long work, the value " + value)
      container.set(value)
    })

    consumer.start()
    producer.start()
  }

  // if you run this, you will get a ton of consumer actively waiting; then, at the end, an I have consumer
  // need something better than the while loop
  naiveProdCons()


  /* Synchronized
    - entering a synchronized expression on an object locks the object
      - someObject.synchronized { //code}  <--  will lock the object's monitor
      - monitor is a data structure used interally by the JVM to keep track of which object is locked by which thread
      - once you have locked the object, any other thread that's trying to evlauate the same expression will block until you're done evaluating
      - when you are done, you will release the lock and any other thread is free to evaluate the expression
      - only AnyRefs can have synchronized blocks (Int, Boolean do not)
      - general principles
        - make no assumptions about which thread gets the monitor first
        - keep locking/sync to a minimum (performance concerns)
        - maintain thread safety at ALL times in parallel applications (or else very nasty bugs)

  wait() and notify(): this is the whole JVM thread communication model
    - wait()-ing on an object's monitor suspends you (the calling thread) indefinitely
    -
      // thread1
      val someObject = "hello"
      someObject.synchronized {   <--- one thread reaches synch expression first; lock the object's monitor with some thread
        //..code part 1           <---- thread executes code before wait()
        someObject.wait()         <---- when it calls wait(), it will release the lock on the monitor and.. suspend at this point
        //..code part 2           <---- when thread allowed to proceed (e.g. due to notify), lock the monitor again and continue
      }

      // thread2 (note: if you want to signal all running threads, use notifyAll)
      someObject.synchronized {   <--- lock object's monitor
        //..code                  <--- thread executes code
        someObject.notify()       <---- give signal to ONE of the sleeping threads that are waiting on this object's monitor that they may continue after they acquire the lock on the monitor again (don't know which one)
        //.. more code            <----
      }
  */


  // smarter producer consumer
  // option1: both threads run in parallel -> consumer gets container first -> prints waiting then hits wait -> producer gets monitor and does it's stuff inside the sync block, and then releases -> consumer finishes sync exp and then prints
  // option2 (rare): both threads run in parallel -> producer gets to container sync first
  def smartProdCons(): Unit = {

    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[consumer] waiting...")
      container.synchronized {
        // at the point of waiting, the consumer thread will release the lock on the container and it will suspend until someone else (namely, the producer) will signal the container that they may continue
        container.wait()
      }

      // container must have some value because the only one that can wake me from waiting will be the producer
      println("[consumer] I have consumed " + container.get)
    })

    val producer = new Thread(() => {
      println("[producer] hard at work...")
      Thread.sleep(2000)
      val value = 42

      container.synchronized {
        println("[producer] I'm producing " + value)
        container.set(value)
        container.notify() // gives signal to consumer thread that they may wake up after the producer has exitted the synchronized expression
      }
    })

    consumer.start()
    producer.start()
  }

}
