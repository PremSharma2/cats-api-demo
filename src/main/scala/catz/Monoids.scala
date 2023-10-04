package catz

import cats.Semigroup
import cats.syntax.eq._

object Monoids  extends App {

  //TODO: Here we are discussing a problem which is solved by monoids not by
  // SemiGroup
  //TODO: Problem: ->

  //TODO : -> type class instances for Type [Int]
  import cats.instances.int._
  //TODO : for type Enrichment or pimping
  import cats.implicits.catsSyntaxSemigroup
  val numbers =  (1 to 1000).toList
  // TODO : -> |+| is always associative Lets prove it
/*
  implicit final def catsSyntaxSemigroup[A: Semigroup](a: A): SemigroupOps[A] =
    new SemigroupOps[A](a)
}

//TODO type Enrichment using implicits
implicit class SemigroupOps[A: Semigroup](lhs: A) {
  def |+|(rhs: A): A = Semigroup[A].combine(lhs, rhs)
 */
  val sumLeft= numbers.foldLeft(0)(_ |+| _)
  val sumRight= numbers.foldRight(0)(_ |+| _)

//TODO :-> |+| is always associative will be  proved if , This === hold true
  //TODO:  for the below scenario
  println(sumLeft === sumRight)



  // TODO : -> Define a General API for fold
  //TODO : -> USe of type Enrichment or Extension Methods from Semi Group , or pimping
  // lets import the correct package
  // TODO : Here as we can see that it is not compiling because
  //TODO: foldLeft requires a starting value and in case for type safe or general API
  //TODO : because we cant pass any int value because it is generic method
  // TODO: because T can be anything so u cant pass initial value as Int
  // Todo: or we need to add empty method to Semigroup type class

  /*
  def combineFold[T](list: List[T])(implicit semiGroup: Semigroup[T]):T ={
    list.foldLeft(0)(_ |+| _)
  }
   */
  //TODO : This problem can be solved in Monoids
  //TODO : Any type class which provide the default value for any for Generic type
  // is called Monoid , Monoid is also a SemiGroup which gives flexibility
  // to pass the default value or zero value of generic type to foldLeft
  /*

  TODO: A monoid is a semigroup with an identity.
   TODO: A monoid is a specialization of a semigroup,
  TODO: so its operation must be associative.
   Additionally, combine(x, empty) == combine(empty, x) == x.
   For example, if we have Monoid[String], with combine as string concatenation,
   then empty = "".

 TODO Semigroup and Monoid
   looks   Like that
   trait Semigroup[A] {
  def combine(x: A, y: A): A
}

TODO trait Monoid[A] extends Semigroup[A] {
  def empty: A
  def combine(x: A, y: A): A
}
   */


// TODO : ---------------------------------Monoids-------------------------------------------------


  // TODO Lets import the Monoid type class
   import cats.Monoid
  // TODO : -> type class instance
  //  TODO : -> import cats.instances.int._
  val intMonoidTypeClassInstance= Monoid.apply[Int]
  val combineValue= intMonoidTypeClassInstance.combine(1, 2)
  val zerovalue: Int = intMonoidTypeClassInstance.empty // it will return empty value for int
  // TODO : -> it will return the intuitive zero value for that particular type

   import cats.instances.string._
  val stringMonoidTypeClassInstance= Monoid.apply[String]
  val emtyString: String = stringMonoidTypeClassInstance.empty // empty value for type string
  // TODO : Hence Monoids are extension of Semigroup with Empty method

  // TODO : As Monoid are also Semigroup we can test combine function as well

  val combineString: String = stringMonoidTypeClassInstance.combine("Hello", "Scala")

  // TODO: Lets test Monoids for Options i.e Higher Kinded types
     import cats.instances.option._
  /*
  TODO
    implicit def catsKernelStdMonoidForOption[A: Semigroup]: Monoid[Option[A]] =
    new OptionMonoid[A]
}
   */
// TODO : Monoids are also higher kinded type because they extend MonoidFunctions

  /*
  TODO
     there is type enrichment, but not with higher kinded types
     because we have already type class instance available for Monoid[Option[Int]]
     so the implicit def available  is able to create a Monoid (Monoid[Option[Int]]) type class instance
     of Type Option
     given that there is an implicit Semigroup[Int] in scope
   */
  //TODO  final def apply[A](implicit ev: Monoid[A]): Monoid[A] = ev
  // so compiler will rewrite this
  // TODO : -> Monoid.apply[Option[Int]](implicit instance:OptionMonoid[Option[Int]])(instance1:Semigroup[Int]))

  val optionMonoidTypeClassInstance: Monoid[Option[Int]] = Monoid.apply[Option[Int]]

  /*
     TODO  Companion Object of Monoid type class looks like this
       object Monoid extends MonoidFunctions[Monoid] {
         final def apply[A](implicit instance: Monoid[A]): Monoid[A] = instance
   */

/*
TODO : signature of type class instance of  Monoid[Option[A]]
TODO
 class OptionMonoid [A](implicit A: Semigroup[A]) extends Monoid[Option[A]]{
 def empty: Option[A] = None
  def combine(x: Option[A], y: Option[A]): Option[A] =
    x match {
      case None => y
      case Some(a) =>
        y match {
          case None    => x
          case Some(b) => Some(A.combine(a, b))
        }
    }
TODO As we can see that this instance has SemiGroup type class instance injected
    into it
 */
val noneValue: Option[Int] = optionMonoidTypeClassInstance.empty
  val combine= optionMonoidTypeClassInstance.combine(Option(2),Option.empty[Int])
  println(combine)

  // TODO: use of  extension methods or type Enrichment
  //  for monoids i.e |+| using this import
  // TODO : import cats.syntax.monoid._
/*
TODO
   implicit final def catsSyntaxSemigroup[A: Semigroup](a: A): SemigroupOps[A] =
  new SemigroupOps[A](a)

  and this Semigroup is Semigroup[Option[Int]] so this A will be Option[Int]

  //TODO type Enrichment using implicits
implicit class SemigroupOps[A: Semigroup](lhs: A) {
  def |+|(rhs: A): A = Semigroup[A].combine(lhs, rhs)

 */
  val fancyCombine= Option(3) |+| Option(2)



  // TODO: implement the foldLeft with Monoid
  // TODO : As we can see that monoid has empty method so we can give
  // TODO: starting value as monoid.empty it is generic value depends on type T
  // TODO : usage of Monoids and SemiGroup is  Collapsing a list

  def combineFold[T](list: List[T])(implicit monoid: Monoid[T]):T ={
    list.foldLeft(monoid.empty)(_ |+| _)
  }


  println(combineFold(numbers))
  println(combineFold(List("I", "Like" , "Monoids")))
  println(combineFold(numbers.map(Option(_))))
  //TODO : Exercise combine a list of phone books as Maps[String,Int]
// TODO task is reduce the List[Map] to Map
val phoneBook= List(
  Map(
    "Alice" -> 235,
    "Brent" -> 236 ,
    "Ram" -> 339
  ) ,
  Map(
    "Charlie" -> 444 ,
    "Daniel" -> 222
  ),
  Map(
    "Tina"-> 123
  )
)
  // TODO: to solve this problem we need to
  //  import a Monoid type-class instance of type [Map]


  //TODO: import cats.instances.map._
  import cats.instances.map._

  /*
 TODO
   implicit def catsKernelStdCommutativeMonoidForMap[K, V: CommutativeSemigroup]: CommutativeMonoid[Map[K, V]] =
    new MapMonoid[K, V] with CommutativeMonoid[Map[K, V]]

     TODO
      class  MapMonoid[K, V](implicit V: Semigroup[V]) extends Monoid[Map[K, V]] {
        def empty: Map[K, V] = Map.empty
       def combine(xs: Map[K, V], ys: Map[K, V]): Map[K, V] =
        if (xs.size <= ys.size) {
      xs.foldLeft(ys) {
        case (my, (k, x)) =>
        // my + (k -> Semigroup.maybeCombine(x, my.get(k))
          my.updated(k, Semigroup.maybeCombine(x, my.get(k)))
      }
      } else {
      ys.foldLeft(xs) {
        case (mx, (k, y)) =>
          mx.updated(k, Semigroup.maybeCombine(mx.get(k), y))
      }
    }
   */


  /*
 TODO
      instance : MapMonoid[String,Int] this will be evaluated by the implicit def
      implicit def catsKernelStdMonoidForOption[Int: Semigroup]: MapMonoid[String, Int] =
       new MapMonoid[String, Int](v:Semigroup[Int] )
       so in combineFold compiler will look for implicit def and will make a call to implicit def
       like that we have mentioned above this def will return type cass instance
       of Monoid as MapMonoid

   */
  //TODO  combineFold[Map[String,Int](List[Map[String,Int]])(instance : MapMonoid[String,Int](implicit V: Semigroup[Int])
  println(combineFold(phoneBook))

  //TODO: Exercise 3:-> Shopping Cart and online stores  problem by monoids
  // TODO : Approach to solve this problem will be define custom Monoid type class
  // TODO: and then use combineByFold to aggregate the carts  to the single cart

  case class ShoppingCart(items:List[String],total:Double)


  // TODO Monoid type-class instance of type Monoid[ShoppingCart]
/*
TODO
      @inline def instance[A](emptyValue: A, cmb: (A, A) => A): Monoid[A] = new Monoid[A] {
    override val empty: A = emptyValue
    override def combine(x: A, y: A): A = cmb(x, y)
  }
 */
  implicit val shoppingCartMonoidTypeClassInstance: Monoid[ShoppingCart] =
    Monoid.instance[ShoppingCart](
    ShoppingCart(List.empty, 0.0) ,
    (sa, sb) => ShoppingCart.apply(sa.items ++ sb.items , sa.total + sb.total)
  )



  // TODO : Define an API for chekout the items in shopping
  // TODO: this will aggregate the list of items to the user cart
  def checkout(shoppingCarts:List[ShoppingCart]) : ShoppingCart = {
     combineFold(shoppingCarts)
  }
println(checkout(
  List(
    ShoppingCart(List("Apple","Mango"), 20.0),
    ShoppingCart(List("Milk","Coconut"), 30.0),
    ShoppingCart(List("Biscuit","Bread"), 40.0),
    ShoppingCart(List("Soup","Potato"), 50.0)

  )


))
// TODO : implicit A: Semigroup[A] another shortcut of passing implicit parameter
 abstract class MyOptionMonoid[A](implicit A: Semigroup[A]) extends Monoid[Option[A]]
}
