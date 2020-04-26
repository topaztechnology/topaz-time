package com.topaz.time

import java.util.UUID
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}

import akka.actor.{Cancellable, Scheduler}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

object AkkaTimeTestUtils {

  /**
    * Useful for testing that work done on a scheduler actually happens. For example if something
    * is due to happen every hour you can do `clock += 1 hour` and it will kick off the
    * scheduled job.
    *
    * See the tests for how it's used.
    */
  class MutableSchedulerClock(timeMillis: Long = 0) extends Clock {
    val scheduler: MutableScheduler = new MutableScheduler(this)

    private val time = new AtomicLong(timeMillis)

    def milliTime(): Long = time.longValue()

    def +=(duration: FiniteDuration): Unit = {
      time.addAndGet(duration.toMillis)
      scheduler.tick()
    }

    override def toString: String = time.toString
  }

  private case class Task(
    runAt: Timestamp,
    runnable: Runnable,
    interval: Option[FiniteDuration],
    id: String,
  ) extends Ordered[Task] {

    def compare(t: Task): Int = runAt.compare(t.runAt)
  }

  /**
    * Don't create this class directly. Create a MutableSchedulerClock and access is that way.
    */
  class MutableScheduler private[time](clock: Clock) extends Scheduler {
    private var tasks = Vector[Task]()

    protected[time] def tick(): Unit = synchronized {
      val now = clock.now()

      val (toRun, remaining) = tasks.partition(t => now >= t.runAt)
      tasks = remaining

      toRun.foreach {
        task =>
          task.runnable.run()

          task.interval.foreach {
            duration =>
              tasks :+= task.copy(runAt = now + duration)
          }
      }

      tasks = tasks.sorted
    }

    def scheduleOnce(
      delay: FiniteDuration, runnable: Runnable
    )(implicit executor: ExecutionContext): Cancellable =
      addToTasks(delay, runnable, None)

    def schedule(
      initialDelay: FiniteDuration, interval: FiniteDuration, runnable: Runnable
    )(implicit executor: ExecutionContext): Cancellable =
      addToTasks(initialDelay, runnable, Option(interval))

    private def addToTasks(
      delay: FiniteDuration, runnable: Runnable, interval: Option[FiniteDuration]
    ): Cancellable = synchronized {
      val now = clock.now()
      val id = UUID.randomUUID().toString
      tasks :+= Task(runAt = now + delay, runnable, interval, id)
      tasks = tasks.sorted

      new Cancellable {
        val c = new AtomicBoolean(false)
        def cancel(): Boolean = MutableScheduler.this.synchronized {
          c.set(true)
          tasks = tasks.filterNot(_.id == id)
          true
        }

        def isCancelled: Boolean = c.get()
      }
    }

    override val maxFrequency: Double = 1000
  }
}
