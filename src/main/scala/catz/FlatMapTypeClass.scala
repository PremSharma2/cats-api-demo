package catz

import cats.{Applicative, Apply, FlatMap}

object FlatMapTypeClass {
//TODO : FlatMap type class it is also called Weaker monad because
  // TODO in this family of monads we have stronger monad is Monad Type class
  // TODO : Note : -> Apply extends Semigroupal and Functors
  //TODO cats encode flatMap as type class which accepts the higher Kinded type
trait MyFlatMap[M[_]] extends Apply[M]{
  def flatMap[A,B](fa:M[A])(f: A=> M[B]):M[B]
  //TODO : implementation of ap in-terms of map and FlatMap
  // Here we have implemented in-terms if flat map and Flatmap
  // bcz we have both of these available, where as in Semigroupal
  // we did not have the map and flatMap
  def ap[A, B](wf: M[A => B])(wa: M[A]): M[B] =
    flatMap(wa)(a => map(wf)(f => f(a)))
  //         |  |        /   \     \/
  //         |  |    M[A=>B] A=>B  B
  //         |  |    \_____   ____/
  //       M[A] A =>      M[B]
}
  // TODO : structure of Monad looks like that and this is what cats API follows
  trait MyMonad[M[_]] extends Applicative[M] with MyFlatMap[M]{
   // def pure[A](value :A):M[A] pure comes from Applicatives lets extend Applicatives
    //ETW pattern
    // TODO : this flatMap method is not fundamental to Monad
    // TODO : but it is fundmental to type class FlatMap so we have to remove it form here
    // and lets Monad extend flatMap
    // def flatMap[A,B](fa:M[A])(f: A=> M[B]):M[B]
    // TODO :-> as we know that Applicatives extends Functor so map comes form Functor
    // TODO : so we need to mark it override def because monad implements flatMap
    //  in-terms of map
    override def map [A,B](fa:M[A])(f: A=> B):M[B] =
      flatMap(fa)(x => pure(f(x)))
  }
// TODO : now lets discuss about type enrichment or Extension methods for FlatMap type class
  import cats.syntax.flatMap._ // flatmap extension method
  import cats.syntax.functor._ // map extension method
  // TODO : now we can use for comprehension
/*
TODO
 object ops {
    implicit def toAllFunctorOps[F[_], A](target: F[A])(implicit tc: Functor[F]): AllOps[F, A] {
      type TypeClassType = Functor[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = Functor[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: Functor[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def map[B](f: A => B): F[B] = typeClassInstance.map[A, B](self)(f)


TODO
 trait Ops[F[_], A] extends scala.AnyRef {
    type TypeClassType <: cats.Functor[F]
    val typeClassInstance : Ops.this.TypeClassType
    def map[B](f : A=>B : F[B] = typeClassInstance.map(F[A])(fx)


TODO
    trait Ops[F[_], C] extends scala.AnyRef {
    type TypeClassType <: cats.FlatMap[F]
    val typeClassInstance : Ops.this.TypeClassType
    def self : F[C]
    def flatMap[B](f : C=> F[B]) : F[B] = typeClassInstance.flatMap(fa:F[B])(f)
 */
  //TODO Design an API end point for all kind of higher kinded type
  def getPairs [M[_]:FlatMap] (numbers:M[Int],chars:M[Char]):M[(Int,Char)] ={
    for{
      n <- numbers
      char <- chars
    }yield(n,char)
  }
  //TODO Design an API end point for all kind of higher kinded type
  /*
TODO
 Compiler will rewrite this as follows
 object ops {
    implicit def toAllFunctorOps[F[_], A](target: F[A])(implicit tc: Functor[F]): AllOps[F, A] {
      type TypeClassType = Functor[F]
    } =
    val ops=new AllOps[F, A] {
        type TypeClassType = Functor[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
      ops.map[Int](f : Int=>Int) : F[Int] = typeClassInstance.map(F[Int])(fx)
   */
   def getFlatmap [M[_]] (fa :M[Int])(implicit typeClassInstance: FlatMap[M]) ={
    //TODO : -> ops.map[Int](f : Int=>Int) : M[Int] = typeClassInstance.map(M[Int])(fx)
     fa.map(_+1)
   }

  def main(args: Array[String]): Unit = {
    FlatMap
  }
}
