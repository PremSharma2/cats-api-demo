package catz

import java.util.concurrent.Executors
import cats.Monad
import cats.data.Validated

import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds

object SemiGroupalTypeClass {
//TODO type class Semigroupal
  // which gives product method
  // Monad also extends Semigroupal product in monad comes form Semigroupal
/*
TODO
       The concept of a Semigroupal product comes from the world
       of functional programming and is related
       to category theory.
       It's a way to combine two contexts
       without requiring them to have a specific
       sequential relationship like that provided by a Monad.
      Semigroupal defines product function,
      which produces a pair of (A, B) wrapped in effect F[_] out of F[A] and F[B].
      The Semigroupal typeclass describes a pairing operation
      that is not dependent on the order of the operands,
      unlike a flatMap operation for monads.
 */
  trait MySemiGroupal[F[_]]{
    def product [A,B](fa:F[A],fb:F[B]):F[(A,B)]
  }

   import cats.Semigroupal
   import cats.instances.option._

  val optionSemigroupalTypeClassInstance: Semigroupal[Option] = Semigroupal.apply[Option]
  val aTupledValue: Option[(Int, Int)] = optionSemigroupalTypeClassInstance
                                             .product(Some(2),Some(3))


  val noneTupled: Option[(Int, Nothing)] = optionSemigroupalTypeClassInstance
                        .product(Some(2),None)

  // Lets Test it for Future
  import cats.instances.future._

  implicit val ec:ExecutionContext = ExecutionContext.
    fromExecutorService(Executors.newFixedThreadPool(2))
  val aSemigroupalFuturetypeClassInstance: Semigroupal[Future] =
    Semigroupal.apply[Future]
    val aTupledFuture: Future[(Int, Int)] = aSemigroupalFuturetypeClassInstance
                               .product(Future(3),Future(4))

  import cats.instances.list._

  val alistSemigroupaltypeclassInstance: Semigroupal[List] = Semigroupal.apply[List]
  val tupledList: List[(Int, String)] = alistSemigroupaltypeclassInstance
                     .product(List(1,2),List("a","b"))

 val tuple3: Option[(Int, Int, Int)] = Semigroupal.tuple3(Option(1), Option(2), Option(3))
  val tuple3WithNone: Option[(Int, Int, Int)] =Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int])
  /*
TODO
  def map3[F[_], A0, A1, A2, Z](f0:F[A0], f1:F[A1], f2:F[A2])(f: (A0, A1, A2) => Z)(implicit semigroupal: Semigroupal[F], functor: Functor[F]): F[Z] =
    functor.map(semigroupal.product(f0, semigroupal.product(f1, f2))) { case (a0, (a1, a2)) => f(a0, a1, a2) }
   */


  // res5: Option[(Int, Int, Int)] = None
  val mappedValue= Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _)
  //// res6: Option[Int] = Some(6)

  import cats.instances.option._
  import cats.syntax.apply._     // for tupled and mapN


  /*

TODO
  The tupled method is implicitly added to the tuple of Options.
  It uses the Semigroupal for Option to zip the values inside the Options,
  creating a single Option of a tuple:
   */

  val tupled: Option[(Int, String)] = (Option(123), Option("abc")).tupled

  // res8: Option[(Int, String)] = Some((123, "abc"))
/*
TODO
 Internally mapN uses the Semigroupal
 to extract the values from the Option
 and the Functor to apply the values to the function.
 private[syntax] final class Tuple3SemigroupalOps[F[_], A0, A1,A2](private val t2: Tuple3[F[A0], F[A1],F[A2]]) extends Serializable {

 def mapN[Z](f: (A0, A1, A2) => Z)(implicit functor: Functor[F], semigroupal: Semigroupal[F]): F[Z]
 = Semigroupal.map3(t3._1, t3._2, t3._3)(f)


def map3[F[_], A0, A1, A2, Z](f0:F[A0], f1:F[A1], f2:F[A2])(f: (A0, A1, A2) => Z)(implicit semigroupal: Semigroupal[F], functor: Functor[F]): F[Z] =
    functor.map(semigroupal.product(f0, semigroupal.product(f1, f2))) { case (a0, (a1, a2)) => f(a0, a1, a2) }
 */
  final case class Cat(name: String, born: Int, color: String)

  (
    Option("Garfield"),
    Option(1978),
    Option("Orange & black")
    ).mapN(Cat.apply)
  // res10: Option[Cat] = Some(Cat("Garfield", 1978, "Orange & black"))
  /*
TODO
  There is only one law for Semigroupal: the product method must be associative.
 product(a, product(b, c)) == product(product(a, b), c)
   */

  val add: (Int, Int,Int) => Int = (a, b,c) => a + b + c
  // add: (Int, Int) => Int = <function2>

  (Option(1), Option(2), Option(3)).mapN(add)

  /**
    TODO
         Exercise Lets generalize this API using monad in pure FP way
   */

  def productWithMonads[F[_],A,B](fa:F[A],fb:F[B])(implicit monad:Monad[F]): F[(A, B)] = {

    monad.flatMap(fa)(a => monad.map(fb)(b => (a,b)))

  }


  // TODO or we can do like that by using extension methods
/*

TODO
 object ops {
    implicit def toAllFlatMapOps[F[_], A](target: F[A])(implicit tc: FlatMap[F]): AllOps[F, A] {
      type TypeClassType = FlatMap[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = FlatMap[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }

 TODO
   trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: FlatMap[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def flatMap[B](f: A => F[B]): F[B] = typeClassInstance.flatMap[A, B](self)(f)
  }
  trait AllOps[F[_], A] extends Ops[F, A] with Apply.AllOps[F, A] {
    type TypeClassType <: FlatMap[F]
  }
 */
  import cats.syntax.flatMap._
  import cats.syntax.functor._ // for implicit map

  def productWithMonadsAnExtensionMethods[F[_],A,B](fa:F[A],fb:F[B])(implicit monad:Monad[F]): F[(A, B)] =
    fa.flatMap(a => fb.map(b => (a,b)))

  def productWithMonadsAnExtensionMethodsfor[F[_],A,B](fa:F[A],fb:F[B])(implicit monad:Monad[F]): F[(A, B)] ={
    for{
      a <- fa
      b <- fb
    } yield (a,b)
  }


  // TODO Semigroupal implementation in terms of Monads
  //  Monad extends Semigroupal the product def comes from Semigroupal
  trait MyMonad[M[_]] extends MySemiGroupal[M] {
    def pure[A](value :A):M[A]
    //ETW pattern
    def flatMap[A,B](fa:M[A])(f: A=> M[B]):M[B]

    def map[A, B](bag: M[A])(f: A => B): M[B]

    def product [A,B](fa:M[A],fb:M[B]):M[(A,B)] =

      flatMap(fa)(a => map(fb)(b=> (a,b)))
  }


/*
 TODO
    Note: As we have proved that Monads extends Semigroupal and product of two Monads
    also when we imported the cats.instances.list._ // Monad[List] bcz monad is semigroupal
    and Monad list runs product in terms of for comprehension
    that is why we got cartesian product
    But  this cartesian product is achived using for comprehension i.e using map and flatmap
   and map and flatMap obeys the so called  monads  laws that is
   monad laws enforce to  follows a particular sequence in operation or execution
   So sometimes we might need to combine values without imposing a particular sequence in operation
   so Semigroupal can used where sequencing is not required
   like Validated


 */
  //import cats.data.Validated
  //  //import cats.instances.list._
  /*
  TODO
     It got combined using the combine method of  Semigroup[List]
     because Validated[List[String],Int] and validated requires Semigroup[List]
     instance to combine the values of Invalid(List)
     I mean Semigroupal/Monad type class instance of type
      Validated[List[String],Int] has implemented product diffrently not influneced
      by monadic Laws so it will give correct ans
   */
  type ErrorsOr[T] = Validated[List[String],T]

  val validatedSemigroupaltypeclassinstance: Semigroupal[ErrorsOr] =

    Semigroupal.apply[ErrorsOr]// it requires an implicit instance of Semigroup[List[_]] to combine the List
  val invalidCombination: ErrorsOr[(Nothing, Nothing)] =
        validatedSemigroupaltypeclassinstance
       .product(Validated.invalid(List("Invalid value"))
               ,Validated.invalid(List("Second-Invalid-value")))


// TODO Now this product of two Either monads using semigroupal
  // will get short circuit because product of these two Semigroupal or Monads
    // will be implemented internally via for comprehension i.e map and flatMap
  // and that will follow monadic laws as well show it will shortcircuit
  // and it will not result in cartesian product
  type EitherOrError[T] = Either[List[String],T]
  import cats.instances.either._ // implicit Monad[Either]
  val eitherSemigroupaltypeclassInstance: Semigroupal[EitherOrError] =
    Semigroupal.apply[EitherOrError]

    val anEitherCombination: EitherOrError[(Nothing, Nothing)] =
      eitherSemigroupaltypeclassInstance.
        product(
      Left(List("Something-Wrong","Exception-Occured")),
      Left(List("Key Not found Exception"))
    )

val zipListSemigroupal :Semigroupal[List] = new Semigroupal[List] {
  override def product[A, B](listA: List[A], listB: List[B]): List[(A, B)] = {
    listA.zip(listB)
  }
}

  def main(args: Array[String]): Unit = {

    def abc [F[_],A] (fa:F[A]) :String= "hello"
    abc(List(1,2,3,4))
    println(tupledList)
    println(invalidCombination)
    println(anEitherCombination)
    println(zipListSemigroupal.product(List(1,2),List(3,4)))
  }
}
