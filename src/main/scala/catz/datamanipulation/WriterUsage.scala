package catz.datamanipulation

object WriterUsage {
  /*
TODO
 Logging is something we all need to do.
 There are a many logging frameworks available
 with various layers of abstraction (logback, slf4j, scalalogging etc).
 These libraries are all side effecting.
 As good functional programmers we want defer side effects
 and push them to the boundaries of the system.
 The problem with logging
So what exactly is the problem with logging?
TODO
 Well the textbook answer to this question is that it’s not Referentially transparent
  with all the associated issues that stem from this.
  We can defer our log statements by wrapping everything in a Task or IO:
  def getUser(): User = {
  logger.info("getting user")
}

// becomes
def getUser(): Task[User] = Task {
  logger.info("getting user")
}
We have achieved referential transparency. Job done? Well not quite.
Let’s take a real world example of a multi user system.
Requests are coming in all the time and we want some way to correlate
log entries for the same request.
 The traditional Java style approach would be to use
 ThreadLocal to pass a correlation id along with the
  request and include this in every log statement, perhaps using MDC.

This doesn’t work so well in the Scala world as
we almost certainly cross async boundaries during an operation.
 The solution? pass an implicit correlation id along the call stack:
 A better way
We can turn this problem on it’s head.
Instead of passing the correlation id along our call stack,
we instead return something that also includes logs.
Our psudocode would look something like:

def getUser: Task[(Logs, User)]
At “the end of the world”
 we then write all log entries with our correlation id.
  Assuming some sort of web server it may look like:

for {
  correlationId <- generateCorrelationId // Task[String]
  userAndLogs <- getUser // Task[(Logs, User)]
  (logs, user) = userAndLogs
  _ <- Task {
    MDC.put("correlationId", correlationId)
    logs.foreach(logMessage => logger.info(logMessage))
    MDC.clear()
  }
   }
} yield user

  import cats.data.WriterT
  import cats.instances.vector._
  import org.slf4j.LoggerFactory
  import org.slf4j.event.Level

  import scala.concurrent.Await
  import scala.concurrent.duration._
  object WriterMonad {

    private val Logger = LoggerFactory.getLogger(this.getClass)

    case class User(id: Int)
    case class Order(totalAmount: Int)

    case class LogEntry(level: Level, message: String)

    def getUser(id: Int): WriterT[Task, Vector[LogEntry], User] = {
      val logs = Task(Vector(LogEntry(Level.DEBUG, "getting user")))
      val withResult = logs.map(logs => logs -> User(id))
      // The apply method is a simple way of constructing a writer with a log and result,
       suspended in F
      WriterT(withResult)
    }

    def getOrder(user: User): WriterT[Task, Vector[LogEntry], Order] = {
      val logs = Task(Vector(LogEntry(Level.DEBUG, "getting order")))
      val withResult = logs.map(logs => logs -> Order(100))
      WriterT(withResult)
    }

    def writeLogs(logs: Vector[LogEntry]): Task[Unit] = Task {
      logs.foreach {
        case LogEntry(Level.DEBUG, message) => Logger.debug(message)
        case LogEntry(Level.INFO, message) => Logger.info(message)
        // don't do this in production!
        case LogEntry(_, message) => Logger.debug(message)
      }
    }

    def main(args: Array[String]): Unit = {

      val program = for {
        user <- getUser(1)
        order <- getOrder(user)
      } yield order

      val logged = for {
        logs <- program.written
        result <- program.value
        _ <- writeLogs(logs)
      } yield result

      val eventualResult = logged.runToFuture
      val result = Await.result(eventualResult, 1.second)

      Logger.info(result.toString)
    }

  }
}

The Writer monad can be a good solution to the tricky problem of correlating
log entries togther in a multi user, multi-threaded environment
 */
}