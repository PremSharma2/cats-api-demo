package catz

import cats.implicits.catsStdInstancesForFuture

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds

// option transformer
import cats.data.OptionT
import cats.instances.list._ // fetch an implicit OptionT[List]
import cats.instances.future._
import cats.instances.future._
object MonadTransformers  extends App {

  /*
   TODO
        when you start using multiple monads together,
       things can get complicated. For example,
       let's say you have a function that returns an Option[Int]
       and another function that returns a List[Int].
       If you want to combine these two in a way that works with both Option and List,
       you could end up with a nested structure like List[Option[Int]]
       or Option[List[Int]].
       This is where monad transformers come into play.
       Monad transformers are a design pattern
       that help you work with stacked monads in a more manageable way.
       They essentially allow you to merge
       two monads into a single new monad that combines the behaviors of both.
       A monad transformer is generally a parameterized type
       that takes a monad type as a parameter.
       The transformer itself is also a monad,
       meaning it has pure and flatMap operations that obey the monad laws.

      TODO:
          A common example is OptionT, which is an Option monad transformer.
           It takes another monad F as a parameter
           and wraps it around an Option,
           resulting in the composite monad F[Option[A]].
          final case class OptionT [F[_], A](value: F[Option[A]])
          OptionT is for where nested monad is Option basically
          OptionT provides monadic operations (flatMap, map, etc.)
          that work seamlessly across both the F and the Option.
          def flatMap[B](f: A => OptionT[F, B])(implicit F: Monad[F]): OptionT[F, B] = {
         OptionT {
        F.flatMap(value) {
        case None => F.pure(None)
        case Some(a) => f(a).value
       }
     }
   }

   */
  /*

  }

     def pure[F[_]]: PurePartiallyApplied[F] = new PurePartiallyApplied[F]
   */

  // Use case and need of Monad Transformers
  def sumAllOptions(values : List[Option[Int]]):Int ={1}

    // TODO MyMonad[M[_]]
    //TODO As we can see that companion object of the OptionT
    // accepts the higherkinded type
    // (value: F[Option[A]]  i.e List[Option[Int]]
    // OptionT has map and flatMap as well so for comprehension can be used here
    /* TODO : OptionT map impl
         def map[B](f: A => B)(implicit F: Functor[F]): OptionT[F, B] =
        OptionT(F.map(value)(_.map(f)))
     */
    /*
TODO
    final case class OptionT [F[_], A](value: F[Option[A]])
    F.flatMap(value)(...):
    This part uses the flatMap method on the
    underlying F monad to apply some transformation to the Option[A] inside it.
 _.fold(F.pure[Option[B]](None))(f):
 This is the function passed to flatMap.
  Here, fold is being called on the Option[A] value.
   fold takes two arguments:
     the first is what to return if the Option is None,
     and the second is what to do if the Option is Some(x)
 TODO
     flatMapF —
       This is an operation that allows you to
       apply a function f to the inner A value
       and then flatten the result back into OptionT[F, B].
     def flatMapF[B](f: A => F[Option[B]])(implicit F: Monad[F]): OptionT[F, B] =
      OptionT(F.flatMap(value)(_.fold(F.pure[Option[B]](None))(f)))
     */
    // TODO : final case class OptionT[F[_], A](value: F[Option[A]])

    import cats.data.OptionT
    import cats.instances.list._ //TODO : -> fetch an implicit type-class instance for  Monad , functor type classes
    // it actually means that its a List[Option[Int]]
    /*
     TODO
        final case class OptionT[F[_], A](value: F[Option[A]])
        object OptionT{
         def apply(value: F[Option[A]]):OptionT
         }
         For this case
         TODO
           object OptionT{
           def apply(value: List[Option[A]]):OptionT
           }
     */
  // It is a wrapper over List[Option[Int]]
  //TODO use of for comprehension here
  val optionList: List[Option[Int]] = List(Option(1), Option(2), Option(3))
    val listOfOptions: OptionT[List ,Int] =OptionT.apply(optionList)
  /*
  TODO
       def foldLeft[B](b: B)(f: (B, A) => B)(implicit F: Foldable[F]): B =
    F.compose(optionInstance).foldLeft(value, b)(f)

    def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B

     When you call foldLeft on an OptionT[List, Int],
     it internally calls Foldable[List].compose(optionInstance).foldLeft(optionList, 0)((a, b) => a + b).
     optionInstance here is Foldable[Option] type class instance
     he compose method from Foldable generates
     a new Foldable instance that knows how to handle List[Option[_]].
     It basically layers the folding operation of List and Option.
     Now, the actual foldLeft operation takes place.
     It iterates through each Option inside the List,
      and then each Int inside the Option, summing them up.
      private[cats] trait ComposedFoldable[F[_], G[_]] extends Foldable[λ[α => F[G[α]]]] { outer =>
  def F: Foldable[F]
  def G: Foldable[G]

  override def foldLeft[A, B](fga: F[G[A]], b: B)(f: (B, A) => B): B =
    F.foldLeft(fga, b)((b, ga) => G.foldLeft(ga, b)(f))
}

   */

    val sum: Int =listOfOptions.foldLeft(0)((a, b) => a + b)
  /*
  TODO
        def map[B](f: A => B)(implicit F: Functor[F]): OptionT[F, B] =
         OptionT( F.map(value)(_.map(f)) )
   */
     val rs: OptionT[List, Int] = listOfOptions.map(_*1)

  /*

  TODO
       Takes a function f that transforms a value of type A into an OptionT[F, B].
   Returns a new OptionT[F, B] after applying f
       def flatMap[B](f: A => OptionT[F, B])(implicit F: Monad[F]): OptionT[F, B] =
    flatMapF(a => f(a).value)
    It delegates the call to flatMapF by taking the
    value (i.e., F[Option[A]]) out of the OptionT[F, B] returned by f.
 TODO
   Takes a function f that transforms a value of type A into F[Option[B]].
    Returns a new OptionT[F, B] after applying f.
  def flatMapF[B](f: A => F[Option[B]])(implicit F: Monad[F]): OptionT[F, B] =
    OptionT(  F.flatMap(value)( b => value.fold( F.pure[Option[B]](None) )(f))   )
    or
     OptionT {
        F.flatMap(value) {
        case None => F.pure(None)
        case Some(a) => f(a)
       }
TODO
 here :
      F.flatMap(value):
      Uses the flatMap of the underlying F monad
      to open up value, which is of type F[Option[A]].
      , it applies a function that takes b (which is Option[A]),
      and it unfolds (or unwraps) this Option[A] using .fold:
      F.pure[Option[B]](None):
      If b is None,
      it wraps a None of the target type Option[B] inside the F monad using F.pure.
      f: If b is Some(a), it applies the function f to a. f returns a value of type F[Option[B]].

     OptionT(...): Finally, it wraps this new F[Option[B]] back into an OptionT.

    this is Option fold method
    final def fold[B](ifEmpty: => B)(f: A => B): B =
    if (isEmpty) ifEmpty else f(this.get)
   */
     val rs2= listOfOptions.flatMap(a => OptionT(List.apply(Option(a+1))))

    val listOfOptionChars: OptionT[List ,Char] =OptionT.
                     apply(List(Option('a'), Option('b'), Option('c'),Option.empty[Char]))

    val listOfTuples: OptionT[List, (Int, Char)] = {
      for {
         char <- listOfOptionChars
          int <-  listOfOptions
      } yield (int,char)

    }

  listOfOptionChars.flatMap { char =>
    listOfOptions.map { int =>
      (int, char)
    }
  }

  /*
   def map[B](f: A => B)(implicit F: Functor[List]): OptionT[F, B] =
        OptionT(F.map(value)(_.map(f)))
   */
  listOfOptions.map(_+1)

  println(listOfTuples.value)



  //TODO Most famous Monad Transformer is Either transformer it is used to work with
  //  List[Either[String,Int]] this kind of nested monads
   import cats.data.EitherT
  implicit val ec: ExecutionContext = ExecutionContext.
    fromExecutorService(Executors.newFixedThreadPool(2))
 val listOfEither: List[Either[String, Int]] = List(Left("Somethin Wrong"),Right(43))
  val futureOfEither: EitherT[Future, String, Int] = EitherT.right(Future(45)) // wrap over Future(Right(45))

  // it is wrapper over   List[Either[String,Int]] nested monad
  //final case class EitherT[F[_], A, B](value: F[Either[A, B]])
  // its like
final case class MyT[F[_], Int](value: F[Int])
  val x: MyT[List, Int] =MyT.apply(List(1,2,3))
  final case class MyT1[F[_], A, B](value: F[Either[A, B]])

  MyT1.apply(Future.apply[Either[String,Int]](Right(42)))

  //TODO final case class EitherT[F[_], A, B](value: F[Either[A, B]])
  val listOfEiterT: EitherT[List, String, Int] =
    EitherT.apply(listOfEither)
  // it is wrapper over   Future[Either[String,Int]] nested monad
  val futureOfEiterT: EitherT[Future, String, Int] =
    EitherT.apply(Future.apply[Either[String,Int]](Right(42)))
/*
TODO
 def map[D](f: B => D)(implicit F: Functor[F]): EitherT[F, A, D] = bimap(identity, f)
 def bimap[C, D](fa: A => C, fb: B => D)(implicit F: Functor[F]): EitherT[F, C, D] =
    EitherT(F.map(value)(_.bimap(fa, fb)))
 */

  val rs1 = futureOfEiter.map(_*1)
  /*
TODO
    def flatMap[AA >: A, D](f: B => EitherT[F, AA, D])(implicit F: Monad[F]): EitherT[F, AA, D] =
    EitherT(F.flatMap(value) {
      case l @ Left(_) => F.pure(l.rightCast)
      case Right(b)    => f(b).value
    })
   */

  futureOfEiter.flatMap(x=>EitherT(Future[Either[String,Int]](Right(x*1))))
  //TODO--------------------------------------------------------------------------------------------------------------------------



/*
  TODO : Exercise
      We have multi machine cluster for your businesses which will receive a traffic
      surge following a media  appearance , We measure bandwidths in Units
      i.e amount of traffic in units
      We want to allocate Two of our servers to cope with traffic spike
      We know the current capacity for each server and we know we will hold the
      traffic if sum of bandwidths is > 250
      Note : -> bigger the Unit value larger the bandwidth you will have
      And also we know that total traffic coming is 250 units
 */

  val bandwidthsConfigOfServer = Map(
    "server1.com.EMEA" ->50,
    "server2.com.APAC" ->300,
    "server3.com.USA" ->170
  )
  // it is used  for   Future[Either[String,Int]] nested monad
  ////final case class EitherT[F[_], A, B](value: F[Either[A, B]])
  type AsyncResponseOFThread[T] = EitherT[Future, String, T]  // wrapper over Future[Either[String, T]]

 val dataBaseThread= Future.apply[Either[String,Int]](Left("Server Not Reachable!!!!"))

  val dbThread: Future[Either[String, Int]] =
    Future.successful(Left("Server Not Reachable!!!!"))

  //TODO bandwidth API here Int is bandwidth of server
  def getBandwidth(server:String): AsyncResponseOFThread[Int] ={
    bandwidthsConfigOfServer.get(server) match {
        //Future[Either[String,Int]]
        // trying access the database
      case None => EitherT.left(Future(s"Server $server unreachable"))
      case Some(b) => EitherT.right(Future(b))

    }
  }

  // TODO 1
  // hint: call getBandwidth twice, and combine the results

  // will return //Future[Either[String,Boolean]]
  def canStandWithSurge(s1:String,s2:String):AsyncResponseOFThread[Boolean] ={
    for{
      band1 <- getBandwidth(s1)
      band2 <- getBandwidth(s2)
    } yield band1 + band2 > 250
    // Future[Either[String, Boolean]]
  }
 /*
  def transform[C, D](f: Either[A, B] => Either[C, D])(implicit F: Functor[F]): EitherT[F, C, D] =
    EitherT(F.map(value)(f))
  */

  // TODO
  // hint: call canWithstandSurge + transform
  // TODO Here we are processing the Async Response of the Thread

  def generateTrafficSpikeReport(s1:String,s2:String):AsyncResponseOFThread[String] ={
    // transform will transform Future[Either[String,Boolean]] to
    // Future[Either[String,String]]
    // def transform[C, D](f: Either[A, B] => Either[C, D])(implicit F: Functor[F]): EitherT[F, C, D] =
    //    EitherT(F.map(value)(f))
    canStandWithSurge(s1,s2) .transform{
      case Left(reason) => Left(s"Servers $s1 and $s2 CANNOT cope with the incoming spike: $reason")
      case Right(false) => Left(s"Servers $s1 and $s2 CANNOT cope with the incoming spike: not enough total bandwidth")
      case Right(true) => Right(s"Servers $s1 and $s2 can cope with the incoming spike NO PROBLEM!")

    }
  }
  val result: Future[Either[String, String]] =generateTrafficSpikeReport("server1.com.EMEA","server2.com.APAC").value
  result.foreach(println)

// TODO ----------modified code---------------------------------------------------------
//   val futureOfEiterT: EitherT[Future, String, Int] = EitherT.
//     apply(Future.apply[Either[String,Int]](Right(42)))
  val futureOfEiter: EitherT[Future, String, Int] = EitherT.right(Future(43))
// both statements are same actually here
//val dataBaseThread1= Future.apply[Either[String,Int]](Left("Server Not Reachable!!!!"))
  // both are same
val dataBaseThread1: EitherT[Future, String, Int] = EitherT.left(Future("Server Not Reachable!!!!"))
  // EitherT[Future, String, T]
  def getBandWidthAPIEnhanced(server:String): AsyncResponseOFThread[Int] ={
    bandwidthsConfigOfServer.get(server) match {
      //Future[Either[String,Int]]
      // trying access the database

      case None => EitherT.left(Future("Server Not Reachable!!!!"))
      //Future[Either[,StringInt]]
      case Some(value) => EitherT.right(Future(value))

    }
  }

  def canStandWithSurgeModified(s1:String,s2:String):AsyncResponseOFThread[Boolean] ={
    for{
      band1 <- getBandWidthAPIEnhanced(s1)
      band2 <- getBandWidthAPIEnhanced(s2)
    } yield band1 + band2 > 250

  }

  def generateTrafficSpikeReportEnhanced(s1:String,s2:String):AsyncResponseOFThread[String] ={
    // transform will transform Future[Either[String,Boolean]] to Future[Either[String,String]]
    canStandWithSurgeModified(s1,s2) .transform{
      case Left(undesirableValue) => Left("Server s1 and s2 cannot cope with the incoming spike")
      case Right(false) =>  Left("Server s1 and s2 cannot cope with the incoming spike: not enough total bandwidth")
      case Right(true) => Right("Server s1 and s2 can cope with the incoming spike")

    }
  }
  val resultFuture: Future[Either[String, String]] =generateTrafficSpikeReport("server1.com.EMEA","server2.com.APAC").value
  result.foreach(println)

}
