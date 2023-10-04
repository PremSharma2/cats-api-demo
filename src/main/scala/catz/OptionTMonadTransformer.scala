package catz

object OptionTMonadTransformer  extends App {
  /*
  import cats.data.OptionT
import cats.implicits._
   */
  /*
    TODO Working with nested monads
     Ugly way of working with Nested monads
     As you can see, the implementations of all of these variations are very similar.
     We want to call the Option operation (map, filter, filterNot, getOrElse),
     but since our Option is wrapped in a Future, we first need to map over the Future.
     OptionT can help remove some of this boilerplate.
     It exposes methods that look like those on Option,
     but it handles the outer map call on the Future so we don’t have to:
   */
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Future
  //Creates an already completed Future with the specified result.
  //def successful[T](result: T): Future[T] = Promise.successful(result).future
  val customGreeting: Future[Option[String]] = Future.successful(Some("welcome back, Ram"))

  val excitedGreeting: Future[Option[String]] = customGreeting.map(_.map(_ + "!"))

  val hasWelcome: Future[Option[String]] = customGreeting.map(_.filter(_.contains("welcome")))

  val noWelcome: Future[Option[String]] = customGreeting.map(_.filterNot(_.contains("welcome")))

  val withFallback: Future[String] = customGreeting.map(_.getOrElse("hello, there!"))

  // Todo -------------Working with OptionT wrapper over nested-monads------------

  import cats.data.OptionT
  import cats.implicits._

  /*
  implicit val futureFunctor: Functor[Future] = new Functor[Future] {
  def map[A, B](fa: Option[A])(f: A => B) = fa map f
}
   */
// wrapper over Future[Option[String]]
  //val customGreeting: Future[Option[String]] = Future.successful(Some("welcome back, Ram"))
  val customGreetingT: OptionT[Future, String] = OptionT(customGreeting)
/*
TODO
    OptionT handles nested monad via Monad and Functor Type class
    def map[B](f: A => B)(implicit F: Functor[F]): OptionT[F, B] =
    OptionT(F.map(value)(_.map(f)))

 */
  val excitedGreeting1: OptionT[Future, String] = customGreetingT.map(_ + "!")
  /*
  def filter(p: A => Boolean)(implicit F: Functor[F]): OptionT[F, A] =
    OptionT(F.map(value)(_.filter(p)))
   */
  val firstname: Future[String] = Future.successful("Jane")
  //def liftF[F[_], A](fa: F[A])(implicit F: Functor[F]): OptionT[F, A] =
  // OptionT(F.map(fa)(Some(_))
  val futureofOption: OptionT[Future, String] = OptionT.liftF(firstname)
  // we can fetch the lifted value here i.e nested monad
  // hence it is proved now optiont is wrapper over nested monad
  val x: Future[Option[String]] = futureofOption.value
  /*

   def flatMap[B](f: A => OptionT[F, B])(implicit F: Monad[F]): OptionT[F, B] =
    flatMapF(a => f(a).value)
    for this case F= Future
    and one important thing both Monad implicit object are diffrent
  def flatMapF[B](f: A => Future[Option[B]])(implicit F: Monad[Future[Option[String]]): OptionT[F, B] =
    OptionT(F.flatMap(value)(_.fold(F.pure[Option[B]](None))(f)))
   */
  //val customGreetingT: OptionT[Future, String] = OptionT(customGreeting)
  customGreetingT.flatMap(_ => OptionT.liftF(firstname))

  val withWelcome: OptionT[Future, String] = customGreetingT.filter(_.contains("welcome"))

  val noWelcome1: OptionT[Future, String] = customGreetingT.filterNot(_.contains("welcome"))

  val withFallback1: Future[String] = customGreetingT.getOrElse("hello, there!")

  /*

  Sometimes you may have an Option[A] and/or F[A] and want to lift them into an OptionT[F, A].
  For this purpose OptionT exposes two useful methods, namely fromOption and liftF, respectively.
   E.g.:

   */

  val greetingFO: Future[Option[String]] = Future.successful(Some("Hello"))

  val firstnameF: Future[String] = Future.successful("Jane")

  val lastnameO: Option[String] = Some("Doe")
// def liftF[F[_], A](fa: F[A])(implicit F: Functor[F]): OptionT[F, A] = OptionT(F.map(fa)(Some(_)))
  val ot: OptionT[Future, String] = for {
    g <- OptionT(greetingFO)
    f <- OptionT.liftF(firstnameF)
    l <- OptionT.fromOption[Future](lastnameO)
  } yield s"$g $f $l"
   // and when u asked the transformed value ask the OptionT
  val result: Future[Option[String]] = ot.value

  /*
   TODO Exercise : on OptionT
    Sometimes the operation you want to perform on an Future[Option[String]]
    might not be as simple as
    just wrapping the Option method in a Future.map call.
     For example, what if we want to greet the customer with their custom greeting
     if it exists in database  but otherwise fall back to a default Future[String] greeting?
     Without OptionT, this implementation might look like:
   */
//Creates an already completed Future with the specified result.
  val defaultGreeting: Future[String] = Future.successful("hello, there")

  val customGreeting1: Future[Option[String]] = Future.successful(Some("welcome back, Ram"))
  /*
  def flatMap[S](f: Option[String] => Future[String])(implicit executor: ExecutionContext): Future[S] = transformWith {
    case Success(s) => f(s)
    case Failure(_) => this.asInstanceOf[Future[S]]


    or

    def flatMap[S](f: T => Future[S])(implicit executor: ExecutionContext): Future[S] = {
    import impl.Promise.DefaultPromise
    val p = new DefaultPromise[S]()
    this.onComplete {
      case f: Failure[_] => p complete f.asInstanceOf[Failure[S]]
      case Success(v) => try f(v) match {
        // If possible, link DefaultPromises to avoid space leaks
        case dp: DefaultPromise[_] => dp.asInstanceOf[DefaultPromise[S]].linkRootOf(p)
        case fut => fut.onComplete(p.complete)(internalExecutor)
      } catch { case NonFatal(t) => p failure t }
    }
    p.future
   */

  val greeting: Future[String] = customGreeting1.
    flatMap(custom =>
    custom.map(str => Future.successful(str)).getOrElse(defaultGreeting))
  /*
    def getOrElseF[B >: A](default: => F[B])(implicit F: Monad[F]): F[B] =
    F.flatMap(value)(_.fold(default)(F.pure))
   */
  val customGreeting1T: OptionT[Future, String] = OptionT(customGreeting)
  val greetingWithOptionT: Future[String] = customGreetingT.getOrElseF(defaultGreeting)

  // TODO Lets play with map and flatMap in OptionT
  //customGreeting1T.map()
}
