package catz

import cats.~>
import catz.FreeMonad.Free.{FlatMap, Pure, Suspend, pure}
import catz.FreeMonad.Monad

import scala.collection.mutable
import scala.language.higherKinds
/*
TODO
    Monads are Datastructures with sequential operations capability
    pure(a) => M[A]
    flatMap( A=> M[B] ):M[B]
    In short:
    a way to wrap a plain value into a “wrapper” type
    a way to obtain a wrapper type from another,
    through a specific kind of transformation function

TODO
    Concept of Free Monad:
    In simple terms, it allows you to define
    a series of operations without specifying
    how those operations are implemented.
    Later, you can provide one or more interpreters
    that define how to actually perform those operations.
    This separation is often useful for testing, modularity, and flexibility.

 */
object FreeMonad {
  //monad
  trait Monad[M[_]] {
    def pure[A](a: A): M[A]

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  }

  object Monad {
    def apply[M[_]](implicit monad: Monad[M]): Monad[M] = monad
  }
  //defining Free Monad
  /*
TODO
  The Free monad in Scala can be described by a similar type signature,
  taking a type argument M[_] which is the container exhibiting monadic capabilities,
  and a type argument A which is the value type
  with which that wrapper is defined.
  In other words, a Free monad trait can be written as
  A difference from the “regular” monad is that in this case,
  we shouldn’t really need an instance of Free to build
  a new instance of Free with the pure method.
  So we’ll move the pure method to a companion object,
  while keeping the pure concept intact:

 */


  /*
TODO
    Now, because we have pure and flatMap, we can implement the map method for free
    the Functor’s fundamental method can be expressed in terms of pure and flatMap:
   */

  trait ~>[F[_], G[_]] {
    def apply[A](fa: F[A]): G[A]
  }

  trait Free[M[_], A] {

    import Free._

    def flatMap[B](f: A => Free[M, B]): Free[M, B] = FlatMap(this, f)

    def map[B](f: A => B): Free[M, B] = flatMap(a => pure(f(a)))

    def foldMap[G[_] : Monad](natTrans: M ~> G): G[A] = this match {
      case Pure(a) => Monad[G].pure(a)

      case FlatMap(fa, f) => // need a G[B]
        Monad[G].flatMap(fa.foldMap(natTrans))(a => f(a).foldMap(natTrans))
      //                |      G[A]          |  |  Free[M,B] |
      //                |      G[A]           | A=>                  G[B]       |
      case Suspend(ma) => natTrans.apply(ma)

    }

  }


  object Free {
    def pure[M[_], A](a: A): Free[M, A] = Pure(a)
    //todo we also have a function that turns a regular M[A] into a Free instance.
    // This function is called “lift”,
    // either named liftM or liftM in real libraries, and looks like this:
    def liftM[M[_], A](ma: M[A]): Free[M, A] = Suspend(ma)
    case class Pure[M[_], A](a: A) extends Free[M, A]
    case class FlatMap[M[_],A,B](fa: Free[M, A], f: A => Free[M, B]) extends Free[M, B]
    case class Suspend[M[_], A](ma: M[A]) extends Free[M, A]
  }
  /*
todo
    Ques: Why We Need The Free monad?
    Ans : Sequence computations as datastructures ,
     Then attach the monadic type at the end
 Example :
     Let’s imagine we’re writing a small tool to interact with a custom database
     we have at work. For the sake of simplicity,
     we’ll assume the fundamental operations of the database to be the regular CRUD:
     This suite of operations (some of which can wrap others in real libraries)
     bears the fancy name of “algebra”,
     for the reason that any expression
     composed out of these fundamental types
     through some operators (like nesting or regular methods)
     belong to this group as well.
     It’s easy to imagine an example of an interaction with such a database:
     read something from a key, like the name of a person
    change it, e.g. run a toUppercase on it
    associate this new value to another key
    delete the old key

   */
//DSL
  trait DBOps[A]
  case class Create[A](key: String, value: A) extends DBOps[Unit]
  case class Read[A](key: String) extends DBOps[A]
  case class Update[A](key: String, value: A) extends DBOps[A]
  case class Delete(key: String) extends DBOps[Unit]

  /*
TODO
       This is a sequential suite of operations.
       Because it’s sequential,
       a monadic data type to describe all of these operations can prove very useful.
       However, instead of writing a type class instance for Monad[DBOps]
       and following the tagless-final approach,
       we’re going to use a Free monad:
   */
  // Lift DSL to Free Monad
   type DBMonad[A] = Free[DBOps, A]
/*
TODO
     Because we’re using a Free monad,
     instead of describing combinators of the data types above,
     we’re going to “lift” them to Free through smart constructors:
 */
def create[A](key: String, value: A): DBMonad[Unit] =
  Free.liftM[DBOps, Unit](Create(key, value))

  def get[A](key: String): DBMonad[A] =
    Free.liftM[DBOps, A](Read[A](key))

  def update[A](key: String, value: A): DBMonad[A] =
    Free.liftM[DBOps, A](Update[A](key, value))

  def delete(key: String): DBMonad[Unit] =
    Free.liftM(Delete(key))


/*
TODO
    This program can be completely described
    in terms of the Free monad,
    regardless of what wrapper type we use.
    The only problem is that it’s a description of a computation;
    it does not perform any meaningful work in the world, like, you know, interacting with an actual database.
 */
// business logic is FIXED
def myLittleProgram: DBMonad[Unit] = for { // monadic
  _ <- create[String]("123-456", "Daniel")
  name <- get[String]("123-456")
  _ <- create[String]("567", name.toUpperCase())
  _ <- delete("123-456")
} yield () // description of a computation
/*
TODO
   What does “interpretation” even mean?
   Interpreting a program means transforming
   this abstract program written in terms of the
   Free monad into another data type
   that actually performs the computations when evaluated.
   A good candidate for such a data structure is,
   for example, the Cats Effect IO.
   However, we can pick another data type of our choosing, with the meaning of
   This is why the Free monad has another operation
   that can “evaluate” an instance of Free to one of these data types.
    The operation is called foldMap and looks like this:
 */
/*
TODO
    The IO type encapsulates computations that evaluate to A,
    with potential side effects.
    The IO data type is a monad,
     meaning that we can create a Monad instance for it:
 */
  case class IO[A](unsafeRun: () => A)
  object IO {
    def create[A](a: => A): IO[A] = IO(() => a)
  }

  implicit object IoMonad extends Monad[IO]{
    override def pure[A](a: A): IO[A] = IO(() => a)

    override def flatMap[A, B](ma: IO[A])(f: A => IO[B]): IO[B] = IO(() => f(ma.unsafeRun()).unsafeRun())
                                                                             //|    A     |
                                                                         //|      IO[B]    |
  }                                                                      //|        B                  |


  /*
TODO
    First of all,
    what’s a natTrans and the ~> symbol?
    The concept of “natural transformation” is a higher-kinded
    Function1 type that looks like this:
   */
  val myDB: mutable.Map[String, String] = mutable.Map()
  // TODO replace these with some real serialization
  def serialize[A](a: A): String = a.toString
  def deserialize[A](value: String): A = value.asInstanceOf[A]

  //transformation
  val dbOpsIO: DBOps ~> IO = new (DBOps ~> IO) {

    override def apply[A](fa: DBOps[A]): IO[A] = fa match {
      case Create(key, value) => IO.create { // actual code that uses the database
        println(s"insert into people(id, name) values ($key, $value)")
        myDB += (key -> serialize(value))
        ()
      }
      case Read(key) => IO.create {
        println(s"select * from people where id=$key limit 1")
        deserialize(myDB(key))
      }
      case Update(key, value) => IO.create {
        println(s"update people(name=$value) where id=$key")
        val oldValue = myDB(key)
        myDB += (key -> serialize(value))
        deserialize(oldValue)
      }
      case Delete(key) => IO.create {
        println(s"delete from people where id=$key")
        ()
      }
    }
  }
  val ioProgram: IO[Unit] = myLittleProgram.foldMap(dbOpsIO)

  def main(args: Array[String]): Unit = {
    ioProgram.unsafeRun() // PERFORMS THE ACTUAL WORK
  }
}


