package catz

import cats.Monad

import scala.language.higherKinds

object UsingMonads extends App {
import cats.instances.list._
  //def apply[F[_]](implicit instance : cats.Monad[F] // instance in scope) :instance
  val monadListtypeClassInstance: Monad[List] = Monad.apply[List]
  val list: List[Int] =monadListtypeClassInstance.pure(2)
  val transformedList: List[Int] = monadListtypeClassInstance.flatMap(list)(x =>List(x,x+1))

/*
TODO
  One of the reasons to use Either type constructor is not to create any surprising output
 when running the program. Without Either, your function is not predictable.
 You may encounter some unexpected side effects when an exception is thrown.
 For instance, instead of blows up the exception unexpectedly,
 Either can return a Left (failure case) or Right (success case).
 Therefore, the function is predictable to the caller,
  letting the caller knows what potential result may happen.
  Try[A] is isomorphic to Either[Throwable, A].
  In other words you can treat a
  Try as an Either with a left type of Throwable,
  and you can treat any Either that has a left type of Throwable as a Try.
  It is conventional to use Left for failures and Right for successes.

TODO
 Of course, you can also use Either more broadly,
 not only in situations with missing or exceptional values.
 There are other situations where
 Either can help express the semantics of a simple union type (where value is one of two types).

TODO
 Semantically, you might use Try to indicate that the operation might fail.
 You might similarly use Either in such a situation,
 especially if your "error" type is something other than
 Throwable (e.g. Either[ErrorType, SuccessType]).
 And then you might also use Either when you are operating
 over a union type (e.g. Either[PossibleType1, PossibleType2]).
 */
  //TODO : Right Hand type is desirable value and the Left hand type is undesirable value
  // TODO because either returns either Left or Right case class instance


  /*
  TODO
     Represents a value of one of two possible types (a disjoint union.)
    Instances of Either are either an instance of Left or Right.
    A common use of Either is as an alternative to Option for dealing
     with possible missing values.
   In this usage, None is replaced with a Left which can contain useful information.
   Right takes the place of Some.
    Convention dictates that Left is used for failure and Right is used for success.

    TODO
      val in = Console.readLine("Type Either a string or an Int: ")
     val result: Either[String,Int] = try {
      Right(in.toInt)
       } catch {
      case e: Exception =>
        Left(in)

  final case class Left[+A, +B](a: A) extends Either[A, B] {
  def isLeft = true
  def isRight = false
}


final case class Right[+A, +B](b: B) extends Either[A, B] {
  def isLeft = false
  def isRight = true
}
     }
   */
  val aManualEither: Either[String,Int] = Right(42)

  //TODO: Most general use case we usually see is by using type alias here

  type LoadingOr[T] = Either[String,T]
  type ErrorOr[T] = Either[Throwable,T]

  //TODO this syntax LoadingOr[T] looks like monad yes Either is Monad
  //TODO : Lets prove it Either is Monad

  /*
  TODO
    This is where the right bias came into the picture.
    Right biased means that functions such as map and flatMap only apply to the
     "right side" or the "happy scenario", leaving the other side untouched
      We  have to use "right projectable" to make it right-biased and flat-mappable
   */
  val either1: Either[Exception, Int] = Right.apply(1)
  val either2: Either[Exception, Int] = Right(2)
  val x: Either.RightProjection[Exception, Int] =either1.right
  either1.flatMap(e1 =>either2.map(e2 => (e1,e2)) )
  //todo: forComprehension
  for {
    one <- either1
    two <- either2
  } yield one + two





  //TODO ------------------------------Monad-Error Type class For either Monad ----------------------------------------------------


  // TODO We can import the Monad type class instances for Either
   import cats.instances.either._
  /*
 TODO
   MonadError[F[_], E]:
   This is a type class that abstracts over "monadic" data types F
   that can model computations that can fail with an error of type E.
   It extends the simpler Monad[F] type class.
   This is an implicit method that defines instances of
   MonadError and Traverse for Either[A, *].
   The asterisk (*) is a type placeholder,
   indicating that this works for any specific Either[A, B].


TODO
  implicit def catsStdInstancesForEither[A]: MonadError[Either[A, *], A] with Traverse[Either[A, *]] =
    new MonadError[Either[A, *], A] with Traverse[Either[A, *]] {
      def pure[B](b: B): Either[A, B] = Right(b)

      TODO: flatMap Impl
         def flatMap[B, C](fa: Either[A, B])(f: B => Either[A, C]): Either[A, C] =
          fa.flatMap(f)

 */
/*
Todo
    Internal mechanics of type class instance resolution from apply method
    Monad.apply[LoadingOr]:
    Here you're asking for a Monad instance for LoadingOr,
    which is Either[String, *].
    The compiler will look for an implicit Monad[Either[String, *]].
    implicit def catsStdInstancesForEither[A]: MonadError[Either[A, *], A] with Traverse[Either[A, *]] =
    ...: Cats already provides a Monad instance for Either[A, *].
    Since MonadError extends Monad, this instance can be used.
    The A in catsStdInstancesForEither[A] becomes String
    when you ask for a Monad[LoadingOr] because LoadingOr[T] = Either[String, T].
    Monad.apply[LoadingOr] fetches this specialized Monad instance
    for Either[String, *]. With it,
    you can use all the methods that a Monad provides,
    like pure, flatMap, etc., but specialized to work with Either[String, *].
 */
  //todo : type LoadingOr[T] = Either[String,T]
//todo: val eitherMonadTypeClassInstance1: Monad[Either] = Monad.apply[Either] // (catsStdInstancesForEither[String])
  // def apply[F[_]](implicit instance : cats.Monad[F]) : cats.Monad[F]
  val eitherMonadTypeClassInstance: Monad[LoadingOr] = Monad.apply[LoadingOr] // (catsStdInstancesForEither[String])

  val anEither: LoadingOr[Int] = eitherMonadTypeClassInstance.pure[Int](42) // Loading[Int]==Right(45)

     println(anEither.isRight)
   anEither.right
   val mappedEither: Either[String, String] =anEither.map[String](int=> int.toString)


  /*
 TODO
  def map[B1](f: B => B1): Either[A, B1] = this match {
    case Right(b) => Right(f(b))
    case _        => this.asInstanceOf[Either[A, B1]]
  }

TODO
  implicit def catsStdInstancesForEither[A]: MonadError[Either[A, *], A] with Traverse[Either[A, *]] =
    new MonadError[Either[A, *], A] with Traverse[Either[A, *]] {
      def pure[B](b: B): Either[A, B] = Right(b)

      TODO: flatMap Impl
         def flatMap[B, C](fa: Either[A, B])(f: B => Either[A, C]): Either[A, C] =
          fa.flatMap(f)
 */


  //type LoadingOr[T] = Either[String,T]
  //val eitherMonadTypeClassInstance: Monad[LoadingOr] = Monad.apply[LoadingOr] // (catsStdInstancesForEither[String])

  val errorMessage: String = "Loading Not Desirable Value of Either"

  val fx: Int => Either[String, Int] = {
    case n if n % 2 == 0 => Right(42)
    case _ => Left(errorMessage)
  }

  val aTransformedEither: LoadingOr[Int]=eitherMonadTypeClassInstance.flatMap[Int,Int](anEither)(fx)


//TODO ------------------Exercises on Either Monad----------------------------------------------------------------

  // TODO :Exercise for Custom types
  case class OrderStatus(orderID: Long , status:String)
  // TODO: Here this API here will return OrderStatus
  //  or will return String value stating status not found

  def getOrderStatus(ordreId:Long):LoadingOr[OrderStatus] =
    Right(OrderStatus(101,"ReadytoShip"))

// Either[String,String]
  def trackLocation(orderStatus:OrderStatus): LoadingOr[String] =
    if(orderStatus.orderID>1000) Left("Not available yet !!!!")
    else Right("Amsterdam")

  //TODO: to combine these APIS here best way is flatMap ....i.e o/p of one is i/p of another
  //TODO : considering a fact that we are dealing with monads
  //TODO trackLocation is function we used to implement ETW pattern for monads
  /*
  def flatMap[AA >: A, Y](f: B => Either[AA, Y]) = e match {
      case Left(a) => Left(a)
      case Right(b) => f(b)
    }
   */


  /*
TODO
  trait MyMonadError[F[_], E]
  implicit def catsStdInstancesForEither[A]: MyMonadError[Either[A,*], A] =
    new MyMonadError[Either[A,*], A] {
      // Define flatMap using either's flatten method
       def flatMap[A,B, C](fa: Either[A, B])(f: B => Either[A, C]): Either[A, C]  =
        fa.flatMap(f)
      // Reuse this definition from Applicative.it gives the value wrapped into Monad
       def pure[B](b: B): Either[A, B] = Right(b)
    }

 */
  // val eitherMonadTypeClassInstance: Monad[LoadingOr] = Monad.apply[LoadingOr](catsStdInstancesForEither[String]())
val orderLocation: LoadingOr[String] = eitherMonadTypeClassInstance.
           flatMap[OrderStatus,String](getOrderStatus(ordreId = 101))(trackLocation)

  //TODO  : To use for comprehension we need to import the extension methods
  val orderStatus: LoadingOr[OrderStatus] = getOrderStatus(101)


  /*
 TODO:  Using implicit type Enrichment by using implicit class Ops[F[_]]
   */
  /*
  implicit class Ops[F[_], C] extends scala.AnyRef {
    type TypeClassType <: cats.FlatMap[F]
    val typeClassInstance : Ops.this.TypeClassType TODO:  type class instance in scope
    def self : F[C]
    def flatMap[B](f : scala.Function1[C, F[B]]) : F[B] = { /* compiled code */ }
   */
  /*
  TODO
   implicit final def catsSyntaxFlatMapOps[F[_]: FlatMap, A](fa: F[A]): FlatMapOps[F, A] =
    new FlatMapOps[F, A](fa)
    -----------------------------------------------------------------------------------
    TODO
      final class FlatMapOps[F[_], A](private val fa: F[A]) extends AnyVal {
     def flatMap=[B](f: A => F[B])(implicit F: FlatMap[F]): F[B] = F.flatMap(fa)(f)

    you write this and compiler wil transform this into as shown below eg
    getOrderStatus(ordreId = 101).flatMap(os => trackLocation(os))
  TODO : compiler re writes our code Like this
   here  fa=getOrderStatus(ordreId = 101)
  todo  eg : new FlatMapOps[Either[OrderStatus]](fa).flatMap(f: OrderStatus => Either[String])(implicit F: FlatMap[Either])

 TODO
   def flatMap[A1 >: A, B1](f: B => Either[A1, B1]): Either[A1, B1] = e match {
      case Right(b) => f(b)
      case _        => e.asInstanceOf[Either[A1, B1]]
    }



 TODO    ----------------------type class instance for monad-------------------------------
     implicit def catsStdInstancesForEither[A]: MonadError[Either[A, *], A] with Traverse[Either[A, *]] =
    new MonadError[Either[A, *], A] with Traverse[Either[A, *]] {
      def pure[B](b: B): Either[A, B] = Right(b)

      TODO: flatMap Impl
         def flatMap[B, C](fa: Either[A, B])(f: B => Either[A, C]): Either[A, C] =
          fa.flatMap(f)
   */


import cats.syntax.flatMap._
import cats.syntax.functor._
  val orderLocationBetter: LoadingOr[String] = {
    getOrderStatus(ordreId = 101).flatMap(os => trackLocation(os))
  }
  val orderLocationForComprehension: LoadingOr[String] = for{
     os      <-getOrderStatus(101)
    location <-trackLocation(os)
  } yield location



  val eitherMonad: LoadingOr[OrderStatus] = getOrderStatus(101)

 val transformedEitherMonad: LoadingOr[String] = eitherMonad.flatMap(trackLocation)

//TODO : -------------------------------Exercise--------------------------------------------------------



  //TODO: Exercise Connection Service API for web APP

  case class Connection(host:String, port:String)

  val config=Map(
    "host" -> "localhost",
    "port" -> "1.2.1.0"
  )


  //todo : Type class Every Where we Designed HttpService API inter terms of type class
  // This HttpService type-class  takes Option as input  and perform action on it

  trait HttpService[M[_]]{
   def  getConnection(config:Map[String,String]): M[Connection]
    def issueRequest(connection:Connection , payload:String): M[String]

  }
  //TODO Provide a real implementation for http service with using Option, Either , Future
 // Todo : this is Type-class instance  for type HttpService[Option]
  implicit object OptionHttpService extends HttpService[Option]{
    override def getConnection(config: Map[String, String]): Option[Connection] = {
      for{
        host <- config.get("host")
        port <- config.get("port")
      } yield Connection(host,port)

    }

    override def issueRequest(connection: Connection, payload: String): Option[String] =
      if(payload.length>20) None
      else  Some(s"Request {$payload} has been Accepted!!!")

  }
// TODO :Now Lets call the HttpService API here in functional style
  //TODO here we can use flatmap here because we want transform Option[A] monad to Option[B]
  // i.e Option[Connection] => Option[String] ETW pattern

  val optionHttpResponse: Option[String] = OptionHttpService.getConnection(config).flatMap{
    conn => OptionHttpService.issueRequest(conn,"Http-Payload")
  }

println(optionHttpResponse)

// TODO : Using For Comprehension we can also do that because Option Monad has flatMap

  val responseHttpOptionFor: Option[String] = for{
    conn <- OptionHttpService.getConnection(config)
    response <- OptionHttpService.issueRequest(conn,"Http-Payload")
  } yield response

  println(responseHttpOptionFor)


  //TODO -------------------------------------------------------------------------------------------------------------


  // TODO Implement HttpService for Either Monad typeclass instance for either monad
  // todo : Either[Throwable,Connection]

  implicit object AggressiveHttpService extends HttpService[ErrorOr]{
    override def getConnection(config: Map[String, String]): ErrorOr[Connection] = {
      (config.get("host"), config.get("port")) match {
        case (Some(host), Some(port)) => Right(Connection(host, port))
        case _ => Left(new RuntimeException("Connection could not be established !!!!"))
      }
    }


    val MaxPayloadLength = 20 // Extract magic number to a named constant

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] = {
      if (payload.length > MaxPayloadLength) {
        Left(new RuntimeException(s"Invalid payload: length should not exceed $MaxPayloadLength"))
      } else {
        Right(s"Request {$payload} has been Accepted!!!")
      }
    }

  }



  val htpServiceResponse: ErrorOr[String] = for{
    conn <- AggressiveHttpService.getConnection(config) //  ErrorOr[Connection]
    response <- AggressiveHttpService.issueRequest(conn,"Http-Payload")
  } yield response
  println(htpServiceResponse)

 // TODO ------------------------------------------------------------------------------------------
  // TODO : Lets design an generic API for HttpService Response which will handle all kinds of Monads

  // TODO it is same like this we did before but more Generalized way

  val orderLocationForComprehension1= for{
    os <-getOrderStatus(101)
    location <-trackLocation(os)
  } yield location




  //TODO Lets test this HttpService API
  // import cats.syntax.flatMap._
  //TODO import cats.syntax.functor._

  /*
  TODO
   final class FlatMapOps[F[_], A](private val fa: F[A]) extends AnyVal {
     def flatMap=[B](f: A => F[B])(implicit F: FlatMap[F]): F[B] = F.flatMap(fa)(f)
TODO
    you write this and compiler wil transform this into as shown below eg
    AggresiveHttpService.getConnection(config).flatMap(con =>
    AggresiveHttpService.issueRequest(conn,"Http-Payload"))

  TODO : compiler re writes our code Like this
    here  fa : ErrorOr[Connection]= AggresiveHttpService.getConnection(config)
   eg : new FlatMapOps[ErrorOr[Connection]](fa).
   flatMap(f: Connection => ErrorOr[String])(implicit F: Monad[ErrorOr]) catsStdInstancesForEither[Throwable]

    TODO implicit type class instance in scope
     implicit def catsStdInstancesForEither[A]: MonadError[Either[A, *], A] with Traverse[Either[A, *]] =
     new MonadError[Either[A, *], A] with Traverse[Either[A, *]] {
      def pure[B](b: B): Either[A, B] = Right(b)

      TODO: flatMap Impl
         def flatMap[B, C](fa: Either[A, B])(f: B => Either[A, C]): Either[A, C] =
          fa.flatMap(f)

       Todo Either flatMap implementation
         def flatMap[AA >: A, Y](f: B => Either[AA, Y]) = e match {
         case Left(a) => Left(a)
          case Right(b) => f(b)
    }
   */
// TODO General API using cats
  def httpResponse[M[_]](service:HttpService[M],payload: String)(implicit  monad:Monad[M]): M[String] ={
   val myhttpResponse= for{
      conn <- service.getConnection(config)
      response <- service.issueRequest(conn,payload)
    } yield response
    myhttpResponse
  }
  import cats.implicits.catsStdInstancesForOption
  println(httpResponse(OptionHttpService,"Hello Either!!"))
 println(httpResponse(AggressiveHttpService,"Hello Either!!"))
}
