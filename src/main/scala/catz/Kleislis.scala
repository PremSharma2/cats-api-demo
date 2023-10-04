package catz

object Kleislis {

  val func1 : Int => Option[String] =
    x => if(x%2==0) Some("Validation Passed") else None

  val func2 : Int => Option[Int] = x => Some(x*3)

  //TODO now the task is to compose these two functions
  // TODO func3 = func1 andThen func2

  val plainfunc1 : Int => String = x => if(x%2==0) "Validation Passed" else "Failed"

  val fx: Int => Int=>Int =x => y => x+y

  val plainfunc2 : Int => Int = x =>   x*3
  //def andThen[A](g: R => A): T1 => A = { x => g(f.apply(x)) }

  val plainFunc3: Int => String = plainfunc2 andThen plainfunc1

  // TODO But problem here is that the we cannot compose func1 and func2 because they returns
  // Higher-kinded-type
//val  func3 = func1 andThen func2

  // TODO but thats not the abstract way of working
  val composedFunc3: Option[String] = func2.apply(2).flatMap(func1)


  //TOdo:-> it is failing because o/p of func2 is Option[Int] and
  // i/p to func2 is Int so composing fails here else u need to unwrap it using orElse
  // TODO that will be painful task
  //TODO There is concept of Kleisli in cats which handles these situation in scala
  // TODO Kleisli Represents a function A => F[B]. i.e a wrapper over function

  /*
 TODO
      The Kleisli type is a wrapper around A => F[B],
      where F is some context that is a Monad.
      What helps us with our composition of contextual results,
       is that Kleisli has a compose function with the following signature (simplified for clarity):
      def compose(g: A => F[B], f: B => F[C])(implicit M: Monad[F]): A => F[C]
      What the above signature tells us is that
      we can join together functions that return results in a context F
     (for which we have a Monad instance) with functions that work on the simple uncontextualised value:
       A => F[B] //g
       B => F[C] //f
        A => F[C] //f compose g

   */

  //final case class Kleisli[F[_], -A, B](run: A => F[B]) // this will fetch the implicit FlatMap[Option] type class instance
//func1 : Int => Option[String]
def stringToNonEmptyString: String => Option[String] = value =>
  if (value.nonEmpty) Option(value) else None

  def stringToNumber: String => Option[Int] = value =>
    if (value.matches("-?[0-9]+")) Option(value.toInt) else None

  import cats.data.Kleisli
  import cats.implicits._ //Brings in a Monadic instance for Option

  //todo :  Kleisli[Option, String, String](run1: String => Option[String])
  //todo Kleisli[Option, String, String]
  val stringToNonEmptyStringK: Kleisli[Option, String, String] = Kleisli(stringToNonEmptyString)

  //todo:  Kleisli[Option, String, Int](run2: String => Option[Int])

  val stringToNumberK: Kleisli[Option, String, Int] = Kleisli(stringToNumber) //Kleisli[Option, String, Int]
  //todo: resultant composed function:
  // Kleisli[Option, String, Int](run3: String => Option[Int])

  val pipeline: Kleisli[Option, String, Int] =
    stringToNumberK
    compose
   stringToNonEmptyStringK

  val func1K: Kleisli[Option, Int, String] = Kleisli(func1)
  val func2K: Kleisli[Option, Int, Int] = Kleisli(func2)

  // now we can compose using Kleisli
  //TODO FlatMap[Option] is required bcz
  // Kleisli will apply flatMap to the Kleisli
  // to chain the the Computation in sequence flatmap will be useful
  // Data structure TO COMPOSE THESE FUNCTIONS
  // func2 : Int => Option[Int] inside the andThen function
  // TODO like that a => F.flatMap(func2.apply(a))(b:B => func1.apply(b))
  /*
  //final case class Kleisli[F[_], -A, B](run: A => F[B])

   def andThen[C](k: Kleisli[F, B, C])(implicit F: FlatMap[F]): Kleisli[F, A, C] =
    this.andThen(k.run)

    def andThen[C](f: B => F[C])(implicit F: FlatMap[F]): Kleisli[F, A, C] =
    Kleisli.shift(a => F.flatMap(this.run.apply(a))(f))

    private[data] def shift[F[_], A, B](run: A => F[B])(implicit F: FlatMap[F]): Kleisli[F, A, B] =
    F match {
      case ap: Applicative[F] @unchecked =>
        Kleisli(r => F.flatMap(ap.pure(r))(run))
      case _ =>
        Kleisli(run)
    }
   */
  // This is the abstract form of Resultant composed function
   val function0: Int => Option[String] = func1K.run
  val function2: Int => Option[Int] = func2K.run
  val func3K: Kleisli[Option, Int, String] = func2K andThen func1K
    val function: Int => Option[String] = func3K.run


  // TODO :
  //  Some convenient API to handle Kleisli data-structure
  // TODO
  //  but remember these API exposed by Kleisli is just to
  //  compose the functions in different ways
// tODO
  //for example map composes two functions Kleisli.map(f)
  // i.e one o/p is higherKinded type other is normal value
  // def map[C](f: B => C)(implicit F: Functor[Option]): Kleisli[F, A, C] =
  //    Kleisli(a => F.map(this.run.apply(a))(f))

  // TODO : This is just o/p of func2 applied to the function
  //  passed as argument to map function
  //val func2 : Int => Option[Int] = x => Some(x*3)

  val multiply: Kleisli[Option, Int, Int] = func2K.map(_*2)

  val multiplyfunction: Int => Option[Int] = multiply.run




  /*

   TODO
     final case class Kleisli[F[_], -A, B](run: A => F[B])
 TODO
     def flatMap[C, AA <: A](f: B => Kleisli[F, AA, C])(implicit F: FlatMap[F]): Kleisli[F, AA, C] =
     Kleisli.shift(a => F.flatMap[B, C](this.run.apply(a))((b: B) => f.apply(b).run.apply(a)))

      private[data] def shift[F[_], A, B](run: A => F[B])(implicit F: FlatMap[F]): Kleisli[F, A, B] =
    F match {
      case ap: Applicative[F] @unchecked =>
        Kleisli(r => F.flatMap(ap.pure(r))(run))
      case _ =>
        Kleisli(run)
    }
    TODO : Important note in flatMap
       it's run(a)
      check the types
     (b: B) => f(b).run(a))
     f(b) is a Kleisli[F, AA, C] where AA <: A
      so in order to obtain a F[C] you pass an argument of type AA which is a
   */

  val function3: Int => Option[Int] = func2K.run
  val function1: Int => Option[String] = func1K.run

  //TODO Kleisli.shift(a => F.flatMap[B, C](this.run.apply(a))((b: B) => f.apply(b).run.apply(a)))

  val chain: Kleisli[Option, Int, String] = func2K.flatMap(_ => func1K)
  val chainResult: Int => Option[String] = chain.run

  // TODO Lets Explore the Identeitytype Id[A]=A
  import cats.Id
  type IntrestingKleisli[A,B] = Kleisli[Id,A,B]// wrapper over f: A => Id[B]
  //f: Int => Id[Int]
  val times: Kleisli[Id, Int, Int] = Kleisli[Id,Int,Int](x => x*2)
  val plus4: Kleisli[Id, Int, Int] = Kleisli[Id,Int,Int](y => y+4)
  /*
    final case class Kleisli[F[_], -A, B](run: A => F[B])
    def map[C](f: B => C)(implicit F: Functor[Option]): Kleisli[F, A, C] =
  //    Kleisli(a => F.map(this.run.apply(a))(a =>f(a)))
    def flatMap[C, AA <: A](f: B => Kleisli[F, AA, C])(implicit F: FlatMap[F]): Kleisli[F, AA, C] =
   Kleisli.shift(a => F.flatMap[B, C](this.run.apply(a))((b: B) => f.apply(b).run.apply(a)))
   TODO : note catch here is that input a will go simultaneously two both of the Kleisli
   Its like two Kleisli executed in parallel
   */
  // f= 4 => t2 + p4
  //val times: Kleisli[Id, Int, Int] = Kleisli[Id,Int,Int](x => x*2)
  /*
  def map[C](f: B => C)(implicit F: Functor[F]): Kleisli[F, A, C] =
    Kleisli(a => F.map(run(a))(f))
   */
  val fz: Int => Kleisli[Id, Int, Int] = (t2:Int) => plus4.map(p4 => t2 + p4)
  /*
   def flatMap[C, AA <: A](f: B => Kleisli[F, AA, C])(implicit F: FlatMap[F]): Kleisli[F, AA, C] =
    Kleisli.shift(a => F.flatMap[B, C](run(a))((b: B) => f(b).run(a)))
   */
/*
TODO
     a => F.flatMap[B, C](run(a)
    When we call run(a), we're applying the current Kleisli instance with a.
    This represents the first computation in our chain,
     producing an output of type F[B].
    This is where it might seem a bit unusual at first.
    When we're chaining another computation (i.e., another Kleisli)
    via the function f: B => Kleisli[F, AA, C],
    this subsequent computation still acts on
    the original input type
    (in this context, AA, which is a subtype or the same type as A).
    The reason we apply a again is that Kleisli allows
    us to have a series of computations where each
    computation has access to the original input as well
    as the result of the previous computation.

    use case:
  TODO
    In practical scenarios, think of scenarios where
    you have an initial input (like user data),
    and multiple steps in your process might need
    to access this original data, even as other
    aspects of the computation change
    based on intermediate results.
    The Kleisli structure encapsulates this pattern.
 */
  val compose: Kleisli[Id, Int, Int] = times.flatMap(fz)

  val forComposed: Kleisli[Id, Int, Int] = for{
    t2 <- times
    p4 <- plus4
  }yield (t2 + p4)

  def main(args: Array[String]): Unit = {
    println(plainFunc3.apply(2))
    val output: Option[String] = chainResult.apply(2)
    println(output)
  }
/*
  TODO
   use of Kleisli in real world
    import cats.Monad
   import cats.mtl.ApplicativeAsk

  TODO
   val makeDB: Config => IO[Database]
   val makeHttp: Config => IO[HttpClient]
   val makeCache: Config => IO[RedisClient]
   then I could combine things as a monad this way:
TODO
    def program(config: Config) = for {
    db <- makeDB(config)
   http <- makeHttp(config)
   cache <- makeCache(config)
  ...
   } yield someResult
but passing things manually would be annoying.
So instead we could make that Config => part of the type
and do our monadic composition without it.

val program: Kleisli[IO, Config, Result] = for {
  db <- Kleisli(makeDB)
  http <- Kleisli(makeHttp)
  cache <- Kliesli(makeCache)
  ...
} yield someResult
If all of my functions were Kleisli in the first place,
then I would be able to skip that Kleisli(...) part of the for comprehension.
 */

}
