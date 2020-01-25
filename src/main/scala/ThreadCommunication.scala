

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



  /*
    Level 2
    - let's say, instead of a container, we have a buffer where producers can produce values and can consumer can consumer values
    - producer produces a value inside one of 3 spots and consumer extracts any value which is new inside this buffer
    - this is more complex because now we have many values and the process can run indefinitely

    - technically speaking: in this scenario, both the consumer and producer should be able to block each other
    - e.g. if buffer is full (i.e. producer has produced enough to fill), producer must block until consumer has finishes extracting some values
    - e.g. if buffer is empty, consumer must block until producer can produce some more

    producer --> [ ? ? ? ] --> consumer
   */

  import scala.collection.mutable
  import scala.util.Random
  def prodConsLargeBuffer(): Unit = {
    val buffer: mutable.Queue[Int] = new mutable.Queue[Int]
    val capacity = 3

    // both of our threads will run forever here
    // consumer tries to pull value and print and then simulate some computation
    val consumer = new Thread(() => {
      val random = new Random()

      while(true) {
        buffer.synchronized {
          if (buffer.isEmpty) {
            println("[consumer] buffer empty waiting...")
            buffer.wait()
          }

          // there must be at least one value in queue
          val x = buffer.dequeue()
          println("[consumer] consumed " + x)

          // hey producer, there's some space now (if you happened to be sleeping)
          buffer.notify() // consumer has finishes extracting a value, so in the case that producer is sleeping (just in case), send a signal
      }

      Thread.sleep(random.nextInt(500))
    }
  })

    val producer = new Thread(() => {
      val random = new Random()
      var i = 0

      while(true) {
        buffer.synchronized {
          if (buffer.size == capacity) {
            println("[producer] buffer is full")
            buffer.wait()
          }

          // there must be at least one empty slot in queue
          println("[producer] producing" + i)
          buffer.enqueue(i)

          // hey consumer, new food for you!
          buffer.notify() // the producer after producing a value, in the case that consumer is sleeping, notify it

          i += 1
        }

        Thread.sleep(random.nextInt(500))
      }
    })

    consumer.start()
    producer.start()
}

  prodConsLargeBuffer()



  /*
    prodCons level 3
    - we have limited capacity buffer but we have multiple producers and multiple consumers
    - don't necessarily have 1-for-1 ratio of producers and consumers


    producer1 -> [ ? ? ? ] -> consumer1
    producer2 -----^ ^------- consumer2
   */

  class Consumer(id: Int, buffer: mutable.Queue[Int]) extends Thread {
    // overriding run method; is originally from runnable trait and over-ridden in Thread
    // when we override run in something extending Thread, we override the run definition
    // when we supply our own runnable to a thread, we are overriding the Runnable interface/trait's method
    // same behavior with different stuff under the hood making it tick
    override def run(): Unit = {
      val random = new Random()

      while(true) {
        buffer.synchronized {
          /*
          scenario:
            - producer produced a value, and two consumers are waiting
            - producer calls notify, which notifies consumer TWO
            - consumer ONE gets out of the wait, dequeues the only value in the buffer, prints, then notifies
            - issue: buffer.notify signals a thread that they can operate on buffer, but this might be another consumer (and queue is now empty)
            - let's say consumer TWO gets the notify; it starts at x and tries to dequeue, but buffer is empty
            - replace `if buffer.empty` with `while buffer.empty`
           */
          while (buffer.isEmpty) {
            println(s"[consumer_$id] buffer empty waiting...")
            buffer.wait()
          }

          // there must be at least one value in queue
          val x = buffer.dequeue()
          println(s"[consumer_$id] consumed " + x)

          // hey somebody, there's some space now (if you happened to be sleeping)
          buffer.notify()
        }

        Thread.sleep(random.nextInt(500))
      }
    }
  }


  class Producer(id: Int, buffer: mutable.Queue[Int], capacity: Int) extends Thread {
    override def run(): Unit = {
      val random = new Random()
      var i = 0

      /*
        one consumer and two producers (reverse of the consumer issue)
          - producer ONE produces an element, queue is full
          - goes to wait, releases lock
          - producer TWO gets the monitor; goes through if statement because queue is full and waits
          - wait brings back producer ONE, which also hits the wait
          - wait signals producer TWO, which is waiting; it then tries to enqueue and now our queue is larger than our capacity
          - which if with while to check if size == capacity
       */
      while(true) {
        buffer.synchronized {
          while (buffer.size == capacity) {
            println(s"[producer_$id] buffer is full")
            buffer.wait()
          }

          // there must be at least one empty slot in queue
          println(s"[producer_$id] producing" + i)
          buffer.enqueue(i)

          // hey someone, new food for you!
          buffer.notify()

          i += 1
        }

        Thread.sleep(random.nextInt(500))
      }
    }
  }


  def multiProdCons(nConsumers: Int, nProducers: Int): Unit = {
    val buffer: mutable.Queue[Int] = new mutable.Queue[Int]
    val capacity = 3

    (1 to nConsumers).foreach(i => new Consumer(i, buffer).start())
    (1 to nProducers).foreach(i => new Producer(i, buffer, capacity).start())
  }

  multiProdCons(3, 3)


  /******exercises
    * 1) think of an example of when notifyAll acts different than notify
    *   - in our application above, it wouldn't matter if we used notifyAll()
    *
    * 2) create a deadlock: a situation where one thread or multiple threads block eachother and they can not continue
    * 3) live lock: a situation where threads cannot continue because they yield execution to eachother in such a way that nobody can continue
    *   - the threads are active, they are not blocked, but they can not continue
  **************/

  // 1 - notifyAll
  def testNotifyAll(): Unit = {
    val bell = new Object

    (1 to 10).foreach(i => new Thread(() => {
      bell.synchronized {
        println(s"[thread $i] waiting ...")
        bell.wait()
        println(s"[thread $i] hooray")
      }
    }).start())

    new Thread(() => {
      Thread.sleep(2000)
      println("[announcer] Rock n Roll")
      bell.synchronized {
        notifyAll()
      }
    })
  }
  // there will be 10 waitings, then 1 rocjk n roll, then 10 hooray printed (in that order)
  // if instead of notifyAll we used notify, only one thread will wake up to say hooray (and program will actually continue running, with the 9 threads being blocked)
  testNotifyAll()

  // 2- deadlock
  // let's imagine a small imaginery society where people salute by bowing
  // when someone bows to you, you bow to them; you only rise when other person has started rising
  // if other person follows same rule, no one will ever rise

  case class Friend(name: String) {
    def bow(other: Friend): Unit = {
      this.synchronized {
        println(s"$this: I am bowing to my friend $other")
        other.rise(this)
        println(s"$this: my friend $other has risen")
      }
    }

    def rise(other: Friend): Unit = {
      this.synchronized {
        println(s"$this: I am rising to my friend $other")
      }
    }

    var side = "right"
    def switchSide(): Unit = {
      if (side == "right") side = "left"
      else side = "right"
    }

    def pass(other: Friend): Unit = {
      while(this.side == other.side) {
        println(s"$this: Oh but please, $other, feel free to pass")
        switchSide()
        Thread.sleep(1000)
      }
    }
  }

  val sam = Friend("Sam")
  val pierre = Friend("Pierre")

  // the bow method can't make the other person rise because it is locked by the thread
  new Thread(() => sam.bow(pierre)).start() // sam's lock, | then pierre's lock
  new Thread(() => pierre.bow(sam)).start() // pierre's lock, | then sam's lock


  // 3 - livelock
  /* scenario:
    - very polite society
    - there is a road with 2 sides
    - if you bump into someone, you are polite so you move to the other side\
    - if you run, it will keep on going forever; keeps printing
    - no threads are blocked, but no threads can continue running
   */
  new Thread(() => sam.pass(pierre)).start()
  new Thread(() => pierre.pass(sam)).start()





















}
