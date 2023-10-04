package catz.datamanipulation

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt
/*
object WriterExercise {
  import cats.data.WriterT
  import cats.implicits._
  case class UserData(id: String, name: String)

  // Mocking a database action monad for demonstration. In a real application,
  // you might be using something like Slick's DBIO.
  type DBIO[A] = Future[A]

//final case class WriterT[F[_], L, V](run: F[(L, V)])
  // Our combined monad using WriterT
  type LoggingDBIO[L, A] = WriterT[DBIO, L, A]

  def fetchUserData(userId: String): LoggingDBIO[List[String], Option[UserData]] = {
    // Mocking a database fetch
    val action: DBIO[Option[UserData]] = Future.successful(Some(UserData(userId, "John Doe")))
//
//   def liftF[F[_], L, V](fv: F[V])(implicit monoidL: Monoid[L], F: Applicative[F]): WriterT[F, L, V] =
  //   WriterT(F.map(fv)(v => (monoidL.empty, v)))
    WriterT.liftF(action).tell(List(s"Fetched data for user $userId"))
  }

  def saveProcessedData(data: UserData): LoggingDBIO[List[String], Boolean] = {
    // Mocking a save action
    val action: DBIO[Boolean] = Future.successful(true)

    WriterT.liftF(action).tell(List(s"Saved processed data for user ${data.id}"))
  }

  def processUserData(userId: String): LoggingDBIO[List[String], Boolean] = for {
    userDataOpt <- fetchUserData(userId)
    processedData = userDataOpt.map(d => d.copy(name = d.name.toUpperCase()))
    result <- processedData match {
      case Some(data) => saveProcessedData(data)
      case None => WriterT.value[DBIO, List[String], Boolean](false).tell(List(s"No data found for user $userId"))
    }
  } yield result

  def main(args: Array[String]): Unit = {
    // Running the action
    val (logs, isSuccess) = Await.result(processUserData("123").run, 2.seconds)

    println(logs)       // List("Fetched data for user 123", "Saved processed data for user 123")
    println(isSuccess)  // true
  }


}


 */