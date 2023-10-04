package catz

import scala.language.higherKinds


/*
 TODO
  Weaker Applicatives means Apply type class: ->
  TODO
    Applicatives are the extension of functors
    MyApplicatves = Functors (map method) + pure method which is fundamental to Applicative
    Also /Monad extends Applicative because they get pure form Applicatives
    also Applicatives extends Semigroupal so they also have product method
    import cats.Applicative
    Applicative----> funtors
   Applicative ----> Semigroupal
    Monad -----> Applicative
 */
object WeakerApplicatives {
  import cats.Functor
  import cats.Semigroupal

  //TODO : Structure of Applicative type class
// TODO cats encode Applicative as tye class which takes HigherKinded type parameter
  trait MyApplicative[F[_]] extends Functor[F] with Semigroupal[F]{
    //TODO :-> fundamental method of Applicative is pure
    def pure[A](a: A):F[A]

    //TODO :->  as applicatives extend Semigroupal it also has product method
    override def product[A, B](fa: F[ A], fb:F[ B]): F[ (A, B)] = {
      val functionWrapper: F[B => (A, B)] = this.map(fa)(a => (b:B)=> (a,b))
      // ap(functionWrapper)(fb)
      // so we can use inbuilt ap as well
      this.ap(functionWrapper)(fb)
    }
    // TODO : -> special method which helps to implement product of two higher kinded types
    //TODO comes from Apply type class
    // as Applicatives are not monads
    def ap [B,A] (functionWrapper : F[B=> (A,B)])(wa:F[B]): F[(A,B)]

    override def map[A, B](fa: F[A])(f: A => B): F[B] = ???
  }


/*
 TODO: ->
     Thing is that  Applicative main fundamental method is pure rest all are auxillary
      because they comes from
     Functor (map) , Semigroupal (product) and ap comes from Apply
     so what cats API did they put these auxiliary methods into Apply trait or type class
     this makes the Applicative clean with pure only so we have created
     Applicative1
     TODO
       Hence this Weaker applicative Apply is basically
       by defination is combo Functor+Semigroupal + ap (method main fundamental method of Apply)
       ap is the only fundamental operation of this Apply type class
 */
// TODO this is the final structure now which is used by scala
  // TODO :-> structure of Apply type class with ap as only fundamental method

  /*
    TODO
      Apply----> Functor
      Apply ----> Semigroupal
   */
  trait MyApply[F[_]] extends Functor[F] with Semigroupal[F]{

  override def map[A, B](fa: F[A])(f: A => B): F[B]

  override  def product[A, B](fa: F[ A], fb:F[ B]): F[ (A, B)] = {
      val functionWrapper: F[B => (A, B)] = this.map(fa)(a => (b:B)=> (a,b))
      // ap(functionWrapper)(fb)
      // so we can use inbuilt ap as well
      this.ap(functionWrapper)(fb)
    }
    // special method which helps to implement product of two higher kinded types
    // as Applicatives are not monads and this is the fundamental method of Apply type class
    def ap [B,A] (functionWrapper : F[B=> (A,B)])(wa:F[B]): F[(A,B)]



  def manpN[A,B,C](tuple:(F[A],F[B]))(f:(A,B) => C) = {
    val tupleWrapper: F[(A, B)] = product(tuple._1,tuple._2)

    map(tupleWrapper){
      case (a,b) => f(a,b)
    }
  }
  }
// TODO Applicative ------> MyApply
  trait Applicative1[F[_]] extends MyApply[F]{
    //most fundamental method of Applicative is pure
    def pure[A](a: A):F[A]
  }

  // Usage of the above concept
  import cats.Apply
   import cats.instances.option._ // for Apply[Option]
  val applyTypeClassInstance: Apply[Option] = Apply.apply[Option]
  /*
   override def ap[A, B](f: Option[A => B])(fa: Option[A]): Option[B] =
        // we are avoiding flatMap/map to avoid allocating more closures
        if (f.isDefined && fa.isDefined) Some(f.get(fa.get))
        else None


        override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] =
        // we are avoiding flatMap/map to avoid allocating more closures
        if (fa.isDefined && fb.isDefined) Some((fa.get, fb.get))
        else None
   */

  // def ap [B,T] (functionWrapper : F[B=> T])(wa:F[B]): F[T]
  val funcApp: Option[Int] =
    applyTypeClassInstance.ap(Some((x:Int) => x+1))(Some(2))

 val productresult: Option[(Int, Int)] =
   applyTypeClassInstance.product(Option(1),Option(2))
  //TODO use of product method and ap method of Apply type class here basically
  //TODO : this works fine along with extension methods
  import cats.syntax.apply._
  val tupleOfOption: (Option[Int], Option[Int]) = (Option(1),Option(2))
  /*
  TODO
   implicit  class Tuple3SemigroupalOps[F[_], A0, A1, A2](private val t3: Tuple3[F[A0], F[A1], F[A2]]) {
   def tupled(implicit invariant: Invariant[F], semigroupal: Semigroupal[F]): F[(A0, A1, A2)] =
   Semigroupal.tuple3(t3._1, t3._2, t3._3)
   TODO
      def tuple3[F[_], A0, A1, A2](f0:F[A0], f1:F[A1], f2:F[A2])(implicit semigroupal: Semigroupal[F], invariant: Invariant[F]):F[(A0, A1, A2)] =
    imap3(f0, f1, f2)((_, _, _))(identity)

//TODO  def identity[A](x: A): A

  def apWith[Z](f: F[(A0, A1, A2) => Z])(implicit apply: Apply[F]): F[Z] = apply.ap3(f)(t3._1, t3._2, t3._3)
  TODO
   def imap3[F[_], A0, A1, A2, Z](f0:F[A0], f1:F[A1 ], f2:F[A2])(f: (A0, A1, A2) => Z)(g: Z => (A0, A1, A2))(implicit semigroupal: Semigroupal[F], invariant: Invariant[F]):F[Z] =
    invariant.imap(semigroupal.product(f0, semigroupal.product(f1, f2))) { case (a0, (a1, a2)) => f(a0, a1, a2) }{ z => val (a0, a1, a2) = g(z); (a0, (a1, a2)) }

    def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]

}
   */
  //  val tupleOfOption: (Option[Int], Option[Int]) = (Option(1),Option(2))
  val optionOfTuple: Option[(Int, Int)] =tupleOfOption.tupled
  val sumOption: Option[Int] = tupleOfOption.mapN(_ + _)
  //  def mapN[Z](f: (A0, A1) => Z)(implicit functor: Functor[F], semigroupal: Semigroupal[F]): F[Z] =
  //  Semigroupal.map2(t2._1, t2._2)(f)
  /*
  def map2[F[_], A0, A1, Z](f0:F[A0], f1:F[A1])(f: (A0, A1) => Z)(implicit semigroupal: Semigroupal[F], functor: Functor[F]): F[Z] =
    functor.map(semigroupal.product(f0, f1)) { case (a0, a1) => f(a0, a1) }
   */
  /*
    TODO
      Implement an mapN function
   */

  def main(args: Array[String]): Unit = {


  }
}
