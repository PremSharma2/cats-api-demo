package catz.datamanipulation

import cats.Id
import cats.data.WriterT
import cats.effect.unsafe.implicits.global

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Writers  extends App {
import cats.data.Writer


  /*
 TODO
    The Writer monad in functional programming is a tool
    to carry around a log or accumulated value alongside a result.
    This can be especially helpful when
    you want to compute a result while also producing a log
    of what happened during the computation.

   TODO
       WriterT:
       This is the monad transformer variant of the Writer monad.
       Monad transformers allow you to stack effects.
       In the case of WriterT,
       it allows you to combine the logging effect of Writer
       with another effect encapsulated by a different monad.

 TODO
       Often, you'll find scenarios
       where you need to combine multiple effects.
       For instance, imagine you need to fetch some data
       asynchronously (an IO or Future effect),
       but there might not be any data to return (an Option effect).
       One way to represent this combined effect would be
       to nest monads: IO[Option[A]].
       But working with nested monads can quickly become cumbersome.
       You'd have to deal with multiple layers of monadic values,
       each of which requires its own set of combinators.

 TODO
      That's where monad transformers come in.
      They allow you to work with stacked/nested effects in a more ergonomic way.
      For our previous example, we could use an OptionT
      transformer with IO to get a structure that
      behaves like both IO and Option but is easier to work with than IO[Option[A]].

 TODO API definition :
      type  Id[V] = V
      final case class WriterT[F[_], L, V](run: F[(L, V)])
      type Writer[L, V] = WriterT[Id, L, V]
           def apply[L, V](l: L, v: V): WriterT[Id, L, V] = WriterT.apply[Id, L, V]((l, v))
           and WriterT looks like this
           Note : F[(L, V)] = (l,v) if F is Id[V] =V i.e identity type
 TODO
         In the case of the WriterT you've seen,
         it's the transformer variant of the Writer monad.
         It allows you to combine the logging effect of Writer
         with another effect (like IO, Option, etc.).
        This stacking lets you produce logs
        while also handling other effects like asynchronicity or optionality.

   */

  //understanding WriterT monad transformer

  import cats.data.OptionT
  import cats.effect.IO
  import cats.implicits._

  import scala.concurrent.ExecutionContext.global

  val maybeResult: IO[Option[Int]] = IO.pure(Some(5))
  val transformed: OptionT[IO, Int] = OptionT(maybeResult)
  val incremented: OptionT[IO, Int] = transformed.map(_ + 1)

  //incremented.value.unsafeRunSync() // Should return Some(6)


  val aWriter: Writer[List[String],Int] = Writer.apply(List("hello scala"),43)

  /*
TODO
  def map[Z](fn: V => Z)(implicit functorF: Functor[F]): WriterT[F, L, Z] =
    WriterT {
      functorF.map(this.run) { z =>
        (z._1, fn(z._2))
      }
    }
   */


  val increasedWriter: WriterT[Id, List[String], Int] = aWriter.map(_+1)
  val rs: Id[(List[String], Int)] = increasedWriter.run

  // TODO and if we want to modify only logs then we will use mapWritten
  /*
    def mapWritten[M](f: L => M)(implicit functorF: Functor[F]): WriterT[F, M, V] =
    mapBoth((l, v) => (f(l), v))

    def mapBoth[M, U](f: (L, V) => (M, U))(implicit functorF: Functor[F]): WriterT[F, M, U] =
    WriterT { functorF.map(this.run)(f.tupled) }
   */
  val aLogWriter: WriterT[Id, List[String], Int] = aWriter.
    mapWritten(_ :+"Found-interesting ")
  /*
  def bimap[M, U](f: L => M, g: V => U)(implicit functorF: Functor[F]): WriterT[F, M, U] =
    mapBoth((l, v) => (f(l), g(v)))
   */
  val aWriterWithBoth: WriterT[Id, List[String], Int] =
    aWriter.
    bimap(_ :+"Found interesting " , _+1)

  val aWriterWithBoth1: WriterT[Id, List[String], Int] =
    aWriter
      .mapBoth{
    (logs,value) =>
      (logs:+ "found something interesting", value+1)
  }

  /*
   TODO
     PAttern gpres like this
     1 define Writer at the beginning
     2 Manipulate them with pure FP map , flatMap
     3 Dump either the value or the Logs
   */
  /*
 TODO
      def value(implicit functorF: Functor[F]): F[V] =
    functorF.map(run)(_._2)
   */
  val finalResult: Id[Int] = increasedWriter.value

  // getting the Logs from the Writer Monad  i.e. Left
  val logsWritten: Id[List[String]] = aWriterWithBoth.written
  //run: Id[(List[String], Int)]
  val bothLogsAndValue: (List[String], Int) = aWriterWithBoth.run
  //bothLogsAndValue
  val writerA= Writer.apply(Vector("a","b"),44)
  val writerB= Writer.apply(Vector("c","d"),40)
  /*
TODO
  // Here problem here is that the:
  // Writer Monad is that is has two components Logs and value
  // we have combined values but Writer flatmap also combines the Logs as well
  // because it has to wrap both value and log into Writer context alias bag
  // so logs will be concatenated automatically
  // and two combine List we have Semigroup type class to combine all elements
  // we have to import the SemiGroup[Vector] so that we can combine two vectors
  // In short Semigroup is required because flatmap
  // take a transformer function to implement ETW pattern
   // so f: A=> Writer[B]
  // like that so in that case we have to merge logs of earlier Writer to new writer

   */
 /*
 TODO Semigroup type class instance for Vector monoid
  implicit object VectorMonoid[A] extends Monoid[Vector[A]] {
    def empty: Vector[A] = Vector.empty
    def combine(x: Vector[A], y: Vector[A]): Vector[A] = x ++ y

  */
  // TODO Lets take a look at flatmap impl
  /*
 TODO
  def flatMap[U](f: V => WriterT[F, L, U])(implicit flatMapF: FlatMap[F], semigroupL:
   Semigroup[L]): WriterT[F, L, U] =
    WriterT {
      flatMapF.flatMap(run) { lv =>
        flatMapF.map(f(lv._2).run) { lv2 =>
          (semigroupL.combine(lv._1, lv2._1), lv2._2)
        }
      }
    }
   */

  // For example
  //  val writerA= Writer.apply(Vector("a","b"),44)
  val fx : Int => Writer[Vector[String],Int]=
  intValue => Writer(Vector("Logs Getting appended !!"),intValue+1)

 val finalValue: (Vector[String], Int) = writerA.flatMap(fx).run

  // Here Semigroup will combine the logs of writerA and writerB
  writerA.flatMap(valueA => writerB.map(valueB=> valueA + valueB))


  // OR
   val compositeWriter = for{
     valueA <- writerA
     valueB <- writerB
   } yield valueA+valueB


  //Lets test this
  println(compositeWriter.run)


  //TODO : ->  What if we want to clear the Logs // Monoid[List[Int]]
  /*
TODO
  Is is used monoid because we dint which Monad is this
  for Example we dint know it is List, Seq , Option ,Vector
  def reset(implicit monoidL: Monoid[L], functorF: Functor[F]): WriterT[F, L, V] =
    mapWritten(_ => monoidL.empty)

TODO
        def mapWritten[M](f: L => M)(implicit functorF: Functor[F]): WriterT[F, M, V] =
    mapBoth((l, v) => (f(l), v))
     def mapBoth[M, U](f: (L, V) => (M, U))(implicit functorF: Functor[F]): WriterT[F, M, U] =
    WriterT(functorF.map(run)(f.tupled))
   */
  aWriter.mapWritten(_=> List.empty)
  val anEmptyWriter= aWriter.reset // clear the logs , and keep the desired value inside

  //TODO
  // Write a function that print thing with Writers

  def countAndSay(n:Int) : Unit ={
    if(n<0) println("starting")
    else {
      countAndSay(n-1 )
      println(n)
    }
  }
  println(countAndSay(10))

  //modify this with writer
  def countAndLog(n:Int):Writer[Vector[String],Int] = {
    if(n<0) Writer(Vector("starting"),0)
    else countAndLog(n-1).flatMap(_ => Writer(Vector(s"$n"),n))
  }

  println(countAndLog(10))
  countAndLog(4).written.foreach(println(_))

  //TODO Exercise Calculate sum
  def naiveSum(n:Int) :Int ={
    if(n<=0) 0
    else {
      println(s"now at $n")
      val lowerSum = naiveSum(n-1)
      println(s"computed sum ${n-1} = $lowerSum")
      lowerSum + n
    }

  }
  /*
  *
  def flatMap[U](f: V => WriterT[F, L, U])(implicit flatMapF: FlatMap[F], semigroupL: Semigroup[L]): WriterT[F, L, U] =
    WriterT {
      flatMapF.flatMap(run) { lv =>
        flatMapF.map(f(lv._2).run) { lv2 =>
          (semigroupL.combine(lv._1, lv2._1), lv2._2)
        }
      }
    }
   */

  //WriterT((Vector(Now at 3, Now at 2, Now at 1, computed sum 0 = 0, computed sum 1 = 1, computed sum 2 = 3),6))

  //  Writer(Vector(s"Now at $n"), n).flatMap(n => sumWithLogs(n-1).
  //  flatMap(
  //  lowerSum => Writer(Vector(s"computed sum ${n-1} = $lowerSum"), n)
  //  .map(_=>(lowerSum + n) )
  def sumWithLogs(n:Int) : Writer[Vector[String],Int] ={
    if(n<=0) Writer(Vector(),0)
    else for{
      _ <- Writer(Vector(s"Now at $n"), n) // println(s"now at $n")
      lowerSum <- sumWithLogs(n-1)
      n <- Writer(Vector(s"computed sum ${n-1} = $lowerSum"), n)// println(s"computed sum ${n-1} = $lowerSum")
    } yield lowerSum + n //   lowerSum + n
  }
  println(sumWithLogs(3))




  def sumWithLogs_v1(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) {
      Writer(Vector(), 0)
    } else {
      Writer(Vector(s"Now at $n"), n)
        .flatMap { currentN =>
          sumWithLogs(n - 1)
            .flatMap { lowerSum =>
              Writer(Vector(s"computed sum ${n - 1} = $lowerSum"), currentN)
                .map(_ + lowerSum)
            }
        }
    }
  }

  def sumWithLogs_v2(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector("Starting"), 0)
    else {
      sumWithLogs_v2(n - 1)
        .flatMap { lowerSum =>
          Writer(Vector(s"Now at $n", s"Computed sum ${n - 1} = $lowerSum"), n )
            .map(_ + lowerSum)
        }
    }
  }

  // Writer with Future
implicit val ec: ExecutionContext = ExecutionContext.
  fromExecutorService(Executors.newFixedThreadPool(2))
  val samFuture1= Future(sumWithLogs(3))
  val samFuture: Future[Writer[Vector[String], Int]] = Future(sumWithLogs(2))
  val logs: Future[Id[Vector[String]]] = samFuture.map(_.written)
  logs.foreach(println(_))
  val logs1: Future[Id[Vector[String]]] = samFuture1.map(_.written)
  // Writers keep separate logs for seprate thread
  println(logs)
  println(logs1)
}
