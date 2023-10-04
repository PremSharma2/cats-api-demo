package catz

import scala.language.higherKinds
import scala.util.Try

/*
TODO :cats api encodes  functors as a type- class
  which provides map method which works on
  As we know that it works on Containers of different kind
  then it will only accept higher kinded type as type value for this type class
 TODO : Higher Kinded types Functor[F[_]] which are in nature functors
 i.e Bag with map
 */
object FunctorTypeClass extends App {
  //TODO Seq is funtor
val aModifed: Seq[Int] = List(1,2,3).map(_ +1)

  //TODO Option is also Functor

  val modifiedOption: Option[Int] = Option(2).map(_ +1)

//TODO : --------------------Cats Functors-----------------------------------------------------

  //TODO : Cats encodes Functor as a type class, cats.Functor,
// TODO :so the method looks a little different. It accepts the initial F[A]
// TODO : as a parameter alongside the transformation function.
  //  TODO Here’s a simplified version of the
  // TODO : Functor Description:  Functor is a type-class which takes type parameter
  //  of Higher Kinded type Like this :
  // ToDO : It has only one fundamental operation called map for the type we passed
  //TODO F for example could be  List, Option, Try etc
  // they all are Functors by definition in nature
  //TODO :  we leave F abstract i.e we will not pass type parameter to F
// TODO : a functor is a type F[A] with an operation map with type (A => B) => F[B]
//  and leave the argument and the return type unchanged,

  // TODO F[_] is abstract  representation of any kind of Bag for example Bag[A](content: A)


  // TODO Type Class
  trait MyFunctor[F[_]] {
    def map[A, B](bag: F[A])(f: A => B): F[B]
  }

  // TODO : Companion Object

object MyFunctor{
  def apply[F[_]](implicit instance : MyFunctor[F]): MyFunctor[F] = instance
}

  //TODO : Example implementation for Functor type-class instance for higher-kinded-type
  // [Option[_]]

  implicit val mycatsStdInstancesForOption: MyFunctor[Option] = new MyFunctor[Option] {
    override def map[A, B](option: Option[A])(f: A => B): Option[B] =
      option.map(f)
  }


  mycatsStdInstancesForOption.map[String,Int](Some("a"))(_.toInt)

   val functorForOption: MyFunctor[Option] = new MyFunctor[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
      case None    => None
      case Some(a) => Some(f(a))
    }
  }

  // TODO : basic expample for type class instance for Functor type class for List type
  implicit val mycatsStdInstancesForList: MyFunctor[List] = new MyFunctor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] =
      fa.map(f)
  }

  val functortypeClassInstance= MyFunctor.apply[Option]

  val rs: Option[Int] =functortypeClassInstance.map(Some(2))(_+1)
  // TODO : Test our own Custom Functor
  //val listTypeClassInstaceforCustomFunctor= MyFunctor.apply[List]
 // println(listTypeClassInstaceforCustomFunctor.map(List(1,2,2))(_+1))


  val functorTypeClasInsatnce= MyFunctor.apply[Option]


  //TODO ---------------------------cats API Functor type class------------------------------------------------------


//TODO lets take a look cats API for functors
  // TODO First of all import the typeclass and type-class instances
  //  of Functor type-class
  // TODO : for the type List
   import cats.Functor
   import cats.instances.list._ // type class instance for type List like i gave example above

  // TODO : def apply[F[_]](implicit instance : cats.Functor[F])
  // TODO here as we can see that apply method takes higherkinded type
  val listTypeClassInstace= Functor.apply[List]

  val incrementedNumbers: List[Int] = listTypeClassInstace.map(List(1,2,3))(_ + 1)
  import cats.instances.option._
  val optionFunctortypeClassInstance = Functor.apply[Option]
  println(optionFunctortypeClassInstance.map(Option(2))(_ * 1))

  // TODO Lets test this Functor Type class for the type Try
  // TODO this is also higherKinded type i.e Functor[Try[Int]]
  import cats.instances.try_._
  val anFunctorTypeClassInstanceForTrytype: Try[Int] = Functor[Try].map[Int,Int](Try(41))(_+1)


  //TODO : need of Functors type class is that when we want to genralize the API
  // TODO : that takes any HigherKinded type alias Monad  and implicit functor type class instance
  // TODO : and try to call map function over it for example
  def do10xList(list: List[Int]) = list.map(_*10)
  def do10xOption(option:Option[Int]) = option.map(_*10)

  //TODO We can generalize this api
/*
TODO : type class instance like below will be implicitly passed to here
TODO
 implicit val mycatsStdInstancesForList = new MyFunctor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] =
      fa.map(f)
  }
 */

  // TODO: Use of functor Type class to expose API or end point which accepts any kind of Monad
  def do10x[F[_]](container:F[Int])(implicit functorTypeClassInstance:Functor[F]):F[Int] ={
    functorTypeClassInstance.map(container)(_+1)
}

  println(do10x(List(1,2,3,4)))
  println(do10x(Option(1)))

  //TODO Binary Tree

  // Binary Tree Reresentation
/*
  Branch(2,Leaf(2),Leaf(2)))

         2
        / \
       /   \
      /     \
     /       \
     2       2


 */


  // TODO : Define a Functor for Binary Tree Data structure
  //this is also ADTs
 sealed trait Tree[+T]

  case class Leaf[+T](value : T) extends Tree[T]
  //TODO: leaf is node of tree which has no branch
//TODO : Branch is also an Tree but it has further branches or leaf nodes only
  case class Branch [+T](value : T , left:Tree[T] , right:Tree[T]) extends Tree[T]

  // TODO : We will create smart constructor instead of using case class constructor
  object Tree{
    def leaf[T](value:T): Tree[T] = Leaf(value)
    def branch[T](value : T , left:Tree[T] , right:Tree[T]): Tree[T] =
      Branch(value,left,right)
  }




//TODO : we will create type class instance for type Functor[Tree[_]]
  implicit object TreeFunctor extends Functor[Tree]{
  override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
    case Leaf(value) => Leaf(f(value))
    case Branch(value, left, right) => Branch(f(value) , map(left)(f), map(right)(f))
  }
  }
  println(do10x[Tree](Branch(2,Leaf(2),Leaf(2))))
  println(do10x(Tree.branch(2,Tree.leaf(2),Tree.leaf(2))))



  //TODO : Extension methods

     import cats.syntax.functor._


  /*
  TODO: Type Enrichment API or extension methods or imp the library API is here
    implicit def toFunctorOps[F[_], A](target : F[A])(implicit tc : cats.Functor[F])
    : Functor.Ops[F, A] {
TODO
  implicit trait Ops[F[_], A] extends scala.AnyRef {
    type TypeClassType <: cats.Functor[F]
    val typeClassInstance : Ops.this.TypeClassType
    def self : F[A]
    def map[B](f : A => B) : F[B] = typeClassInstance.map(F)(f)
   */
  val tree: Tree[Int] = Tree.branch(2,Tree.leaf(2),Tree.leaf(2))
  tree.map(_ + 1)


  // TODO : Exercise Create Shorter Version of do10x def
  //TODO : using the cats API typeEnrichment we can modify our code like that
  import cats.syntax.functor._
  def smartDo10x[F[_]](container:F[Int])(implicit functortypeClassInstance:Functor[F]):F[Int] ={
    container.map(_+1)
  }

  println(smartDo10x(Tree.branch(2,Tree.leaf(2),Tree.leaf(2))))
}
