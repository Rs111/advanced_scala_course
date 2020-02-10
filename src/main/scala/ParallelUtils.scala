import java.util.concurrent.atomic.AtomicReference

import scala.collection.parallel.{Task, TaskSupport}

object ParallelUtils extends App {

  // 1 - parallel collections
  // calling par on a list means that operations on them are handled by multiple threads at the same time
  // can create for Seq, Vector, Array, Map, hash maps, trie maps, Set, hashed set, trie set
  // parallelizing collections increases performance with large collections
  // with small collections, serial collections are faster due to lower overheard (starting and stopping threads is expensive)
  val parList = List(1,2,3).par
  import scala.collection.parallel.immutable.ParVector
  val aParVector = ParVector[Int](1,2,3)

  def measure[T](operation: => T): Long = {
    val time = System.currentTimeMillis()
    operation
    System.currentTimeMillis() - time
  }

  val list = (1 to 10000000).toList
  val serialTime = measure {
    list.map(_ + 1)
  }
  val parTime = measure {
    list.par.map(_ + 1)
  }

  /*
    Parallel collections operate on the Map-reduce model
    - say we want to apply an operation on a parallel collection
    - what a parallel collection will do:
      - split the elements into chunks which will be processed independently by a single thread
        - this is done with a thing called Splitter, which is in the par collections library
      - operation takes place on each chunk and every chunk is processed by a seperate thread
      - results are re-combined with a thing called Combiner (reduce step)
   */

  /** watch out for these things when using par collections **/

  /*
     - Notes:
      - careful when using reduce on par collections; order not maintained
      - not sure if this point is actually valid
   */
  println(List(1,2,3).reduce(_ - _))
  println(List(1,2,3).par.reduce(_ - _))

  /*
    - Synchronization:
      - sometimes you need synchronization; not on the collections themselves, but on the results they act upon
      - e.g. below: the vum var is not locked, may be under race conditions; number 6 is not guaranteed
   */
  var sum = 0
  List(1,2,3).par.foreach(sum += _)

  /*
    Configuring Parallel collections
    - when you say List(1,2,3).par you lose all control over how many threads, how the list(1,2,3) is managed by those threads, etc
    - configuring a par collection is done with a member called task support
    - what can you put inside task support?
      - new ForkJoinTaskSupport(new ForkJoinPool(2))
      - ThreadPoolTaskSupport - deprecated version of ForkJoinTaskSupport
      - ExecutionContextTaskSupport(EC) - takes execution context as parameter
      - can also create your own task support
   */
  import scala.collection.parallel.ForkJoinTaskSupport
  import scala.concurrent.forkjoin.ForkJoinPool
  aParVector.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(2)) // 2 is # of threads

  // you almost certainly don't need this
  aParVector.tasksupport = new TaskSupport {

    // schedules a thread to run in parallel
    override def execute[R, Tp](fjtask: Task[R, Tp]): () => R = ???

    // does the same thing as above, but it's going to block until a result is available; so it's basically going to wait for the thread to join
    override def executeAndWaitResult[R, Tp](task: Task[R, Tp]): R = ???

    // number of cores it should run
    override def parallelismLevel: Int = ???

    // the manager that manages the threads; can be a forkJoinPool, an execution context (totally different thing), anything you want
    override val environment: AnyRef = ???
  }





  /***********Atomic operations and references ********************/
  // atomic operation is one that can't be divided; either it runs fully or not at all
  // in a multithreaded context, an atomic operation is one that can't be intercepted by another thread
  /*
  -https://stackoverflow.com/questions/8772184/how-to-explain-atomic-actions
  Regarding concurrency, atomicity rather means that when a thread modifies the state of some object (or set of objects), another thread can't see any intermediary state. Either it sees the state as it was before the operation, or it sees the state as it is after the operation.

  For example, changing the value of a long variable is not an atomic operation. It involves setting the value of the 32 first bits, and then setting the state of the 32 last bits. If the access to the long variable is not properly synchronized, a thread might see the intermediary state: the 32 first bits have been changed, but the 32 last bits haven't been changed yet.

  The way to implement atomic operations is to use synchronization. synchronization involves using

  the synchronized keyword
  the volatile keyword
  atomic variables (AtomicInteger, etc.)
   */
  val atomic = new AtomicReference[Int](2)

  // all these operations are thread safe
  val currentValue = atomic.get() // thread-safe read; when you extract the atomic reference, no other thread can read or write to tis atomic reference
  atomic.set(4)  // thread safe write

  atomic.getAndSet(5) // extract value and reset

  atomic.compareAndSet(38, 56) // if value is 38, set value to 56; otherwise do nothing

  // functions running on atomic references
  // sets a new value inside with a function and it will return the result of that function applied to the result that the atomic reference currently contains
  atomic.updateAndGet(_ + 1) // thread safe function run and get value
  atomic.getAndUpdate(_ + 1) // gets the initial value and then updates

  // take arguement, take value inside the atomic, sum them up, and then return the result
  atomic.accumulateAndGet(12, _ + _)
  atomic.getAndAccumulate(12, _ + _)








}
