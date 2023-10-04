package catz

import scala.language.higherKinds

object Applicatives {

/*
 TODO
    Applicatives are the extension of functors
    Applicatives = Functors map method + pure method
    Also /Monad extends Applicative because they get pure form Applicatives
    also Applicatives extends Semigroupal so they also have product method
    import cats.Applicative
  Applicative is a type class in the Cats library
    (a Scala library that provides abstractions for functional programming).
    The Applicative type class sits between the Functor
    and Monad type classes in terms of power and expressiveness.
TODO
   Applicative generalizes the notion of applying a function
   within a context (like Option, List, Future, etc.)
   to some value(s) within the same context.

TODO
   Here's what Applicative adds over Functor:
TODO
   Pure: It provides the ability to lift a value into a context.
   Ap (or <*> operator in some languages):
   It allows applying a function wrapped in a context
    to a value wrapped in a context.
 */

  import cats.Applicative
  import cats.instances.list._

  val listApplicativetypeClassInstance: Applicative[List] = Applicative.apply[List]
  val aList: List[Int] = listApplicativetypeClassInstance.pure(2)

  import cats.instances.option._
  val optionApplicativeTypeClassInstance: Applicative[Option] = Applicative.apply[Option]
  val anOption: Option[Int] = optionApplicativeTypeClassInstance.pure(2)

  // TODO pure extension methods for Applicatives or type Enrichment
 import cats.syntax.applicative._
  /*
TODO
  implicit class ApplicativeIdOps[A](private val a: A) extends AnyVal {
  def pure[F[_]](implicit F: Applicative[F]): F[A] = F.pure(a)
}
   */
 val aSweetList: List[Int] = 2.pure[List]
  val aSweetOption: Option[Int] = 2.pure[Option]

  //TODO Monads extends Applicatives pure method of Monads comes from the Applicatives
    // TODo so applicatives are weaker monads so they are rarely used
  // todo Applicatives also extends Functors for map method

  //import cats.Monads
 // Monad

/*
  TODO
        Applicative type class instance for Validated

 TODO
   private[data] class ValidatedApplicative[E: Semigroup] extends  Applicative[Validated[E, *]] {
TODO
   override def map[A, B](fa: Validated[E, A])(f: A => B): Validated[E, B] =
    fa.map(f)


    TODO
      def pure[A](a: A): Validated[E, A] = Validated.valid(a)


      TODO
        def ap[A, B](ff: Validated[E, (A) => B])(fa: Validated[E, A]): Validated[E, B] =
        fa.ap(ff)(Semigroup[E])


  todo
    override def product[A, B](fa: Validated[E, A], fb: Validated[E, B]): Validated[E, (A, B)] =
    fa.product(fb)(Semigroup[E])
}

*/

  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String],T]
  val avalidValue: ErrorsOr[Int] = Validated.valid(2)// its like applicative pure method
  val amodifiedValidated: ErrorsOr[Int] = avalidValue.map(_+1)// map of Functor

  // TODO so we can say that the Validated looks like applicative type class instance

  val validatedApllicative: Applicative[ErrorsOr] = Applicative.apply[ErrorsOr]

  /*
    TOdo Exercise
      Note
      Important thing is that here if you want to implement the product method for applicatives
      you cant do that because you dont have
      flatMap method for Applicative because applicatives are weaker monad
      As applicative is pure + functor map
      so to implement that cats provide this ap method
      so that product can be implemented because some how we need to have
      monad like facility
       def productWithMonads[F[_],A,B](fa:F[A],fb:F[B])(implicit monad:Monad[F]) =
        monad.flatMap(fa)(a => monad.map(fb)(b => (a,b)))
        but we cant have flatmap so ap will be helpful
        SO Applicative can also be a semigroupal now because they are able define product method


 */
  def ap [F[_],B,T] (functionWrapper : F[B=> T])(wa:F[B]): F[T] = ???

  def productWithApplicatives[F[_],A,B] (fa:F[A],fb:F[B])(implicit applicative: Applicative[F]): F[(A,B)] ={
   val functionWrapper: F[B => (A, B)] = applicative.map(fa)(a => (b:B)=> (a,b))
   // ap(functionWrapper)(fb)
    // so we can use inbuilt ap as well
    applicative.ap(functionWrapper)(fb)
  }

  def main(args: Array[String]): Unit = {

  }
}
