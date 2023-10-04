package catz

import cats.Monad
import cats.implicits.catsStdInstancesForList

import scala.language.higherKinds

object ExtensionMethodsForMonads  extends App {

  // TODO Extension method for Monads are pure and flatMap
  // TODO which comes with  Type Enrichment API which is implicit value class
  //TODO : it is the type class instance for type-class Monad[Option]
  /*
  TODO : Like this one
   implicit def optionMonad() =
    new MyMonad[Option] {
      // Define flatMap using Option's flatten method
      override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
        fa.flatMap(f)

      // TODO: Reuse this definition from Applicative.it gives the value wrapped into Monad
            override def pure[A](a: A): Option[A] = Option(a)
    }
   */


  /*
  TODO: Type Enrichment or pimping to convert any val to Monad
  TODO
   trait ApplicativeSyntax {
   implicit final def catsSyntaxApplicativeId[A](a: A): ApplicativeIdOps[A] =
    new ApplicativeIdOps[A](a)
   implicit final def catsSyntaxApplicative[F[_], A](fa: F[A]): ApplicativeOps[F, A] =
    new ApplicativeOps[F, A](fa)
      }

      TODO:
       final class ApplicativeIdOps[A](private val a: A) extends AnyVal {
       def pure[F[_]](implicit F: Applicative[F]): F[A] = F.pure(a)

       or we can say like this

     TODO: Type Enrichment convert Int to Option[Int]
        implicit  class ApplicativeIdOps[A](private val a: Int) extends AnyVal {
       def pure[F[_]](implicit typeClassInstance: Applicative[Option]): Option[Int] =
         typeClassInstance.pure(a)
}
   */

  import cats.implicits.catsStdInstancesForOption
  import cats.syntax.applicative._
// TODO ALl type Enrichment Ops for Int -> Monad are defined  in cats.syntax.applicative._
  val oneOption: Option[Int] = 1.pure[Option]
/*
TODO : For Example Like this Ops and it also take implicit type class instance in scope
    implicit  class ApplicativeIdOps[A](private val a: A) extends AnyVal {
  def pure[F[_]](implicit F: Applicative[F]): F[A] = F.pure(a)
}
 */
  // TODO : Another extension methods or Type Enrichment is flatmap
  //TODO import cats.syntax.flatMap._
  //TODO But flatMap is already there in monad so there is no need to use extension methods
  val transformedOption = oneOption.flatMap(x => (x * 2).pure[Option])

  // TODO : Exercise Extend Monad to add Functor type behaviour or functor properties
  // TODO But we need to implement map by using flatMap
  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]

    def flatMap[A, B](container: M[A])(f: A => M[B]): M[B]

    def map[A, B](container: M[A])(f: A => B): M[B] =
      flatMap(container)(x => pure(f(x)))
  }

  // TODO Custom type class instance of MyMonad
  implicit object OptionMonad extends MyMonad[Option] {
    override def pure[A](value: A): Option[A] = Option(value)

    override def flatMap[A, B](container: Option[A])(f: A => Option[B]): Option[B] =
      container.flatMap(f)

  }

  // TODO :Hence it proves that Monad Extends Functor i.e
  // TODO Monad is also a Functor as well as Applicative

  //TODO import cats.syntax.functor._
  val oneOptionMapped = oneOption.map(_ * 2)
  // TODO calling map over Monad type-class instance  because Monad is Functor so
  //  it has all features of Functor
  val monadOptiontypeClassInstance = Monad.apply[Option]
  monadOptiontypeClassInstance.map(oneOption)(x => x + 1)
  val mymonadOption = OptionMonad
  val myOptionMonadwithValue: Option[Int] = OptionMonad.pure(1)
  val mytransformedoptionMonad: Option[Int] = mymonadOption.map(oneOption)(_ + 1)

  // TODO : now most important feature is that if we have proved that the Monad are Functors SAME
  // TODO : then they ideal candidate for the for comprehension
// TODO because monad has map and flatmap both

  val composedOptionMonad: Option[Int] = for {
    one <- 1.pure[Option] // Some(1)
    two <- 2.pure[Option]  // Some(2)

  } yield one + two

  // TODO testing custom monad

  val composedCustomOptionMonad: Option[Int] = for {
    one <- myOptionMonadwithValue
    two <- mytransformedoptionMonad

  } yield one + two

  import cats.syntax.flatMap._
  import cats.syntax.functor._
  // TODO: Exercise Implement getPairs  using the for comprehension
  /*
  implicit class Ops[F[_], C] {
    type TypeClassType <: cats.FlatMap[F]
    val typeClassInstance : Ops.this.TypeClassType
    def self : F[C]
    def flatMap[B](f : scala.Function1[C, F[B]]) : F[B] = { /* compiled code */ }

    trait Ops[F[_], A] extends scala.AnyRef {
    type TypeClassType <: cats.Functor[F]
    val typeClassInstance : Ops.this.TypeClassType // instance in scope
    def self : F[A]
    def map[B](f : scala.Function1[A, B]) : F[B] = { /* compiled code */ }


   */
//TODO this flatmap of Ops class will internally use the typeclass instance of monad and will call
  // TODO flatmap and in turn will call original flat map of Monad
  //TODO : new Ops[List](list)(implicit monadinstance).flatMap(fx)
  def getPairs[M[_], A, B](bag1: M[A], bag2: M[B])(implicit monad: Monad[M]): M[(A, B)] = {
    bag1.flatMap(a => bag2.map(b => (a, b)))
  }
// TODO for comprehension
  def getPairsFor[M[_], A, B](bag1: M[A], bag2: M[B])(implicit monad: Monad[M]): M[(A, B)] = {
    for {
      a <- bag1
      b <- bag2
    } yield (a, b)

  }
  println(composedCustomOptionMonad)
  println(composedOptionMonad)
  val numbersList= List(1,2,3,4)
  val charsList: List[Char] =List('a','b','c')
  println(getPairs(numbersList,charsList))
  println(getPairsFor(numbersList,charsList))
}