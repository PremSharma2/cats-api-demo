package catz

// TODO : -> Semigroup is a type class that combines the Element of same type
// TODO and what it combines that depends on type of type class instance
/*
TODO : SemiGroup Type class Looks like that

TODO
 trait Semigroup[@sp(Int, Long, Float, Double) A] {
      def combine(x: A, y: A): A
  Associative operation taking which combines two values.
}
 */

//lets import the type class
object SemiGroups  extends App {

  //TODO import the type class
  import cats.Semigroup

  // TODO lets import type class instances for Int type
  import cats.instances.int._
  /*
TODO
      implicit val catsKernelStdGroupForInt: CommutativeGroup[Int] = new IntGroup
}
TODO
 class IntGroup extends CommutativeGroup[Int] {
  def combine(x: Int, y: Int): Int = x + y
  def empty: Int = 0
  def inverse(x: Int): Int = -x
  override def remove(x: Int, y: Int): Int = x - y
 }
   */

  // naturalIntSemiGroupTypeClassInstance is an type class instance
  val naturalIntSemiGroupTypeClassInstance: Semigroup[Int] = Semigroup.apply[Int]

  //TODO calling combine operation on type class instance

  val combineNaturals: Int = naturalIntSemiGroupTypeClassInstance.combine(2,46)
  println(combineNaturals)


  // naturalStringSemiGroupTypeClassInstance is an type class instance
  // TODO import type class instance for the String type
  import cats.instances.string._
  val naturalStringSemiGroupTypeClassInstance: Semigroup[String] = Semigroup.apply[String]
  val combineNaturalStrings= naturalStringSemiGroupTypeClassInstance.combine("Jai","Maa")
  println(combineNaturalStrings)

  // TODO --------------------------------------------------------------------------------


  // TODO : -> use of Semigroup type class  is to reduce the List elements
  // TODO or combine the elements in list
  //TODO : -> this we can enhance by converting into
  //      type Enrichment by making it implicit class

  //TODO : Use case -> General API for Reduction of any type using type class


/*
TODO
 def reduce[A1 >: A](op: (A1, A1) => A1): A1
 Description copied from class: GenTraversableOnce
 Reduces the elements of this collection or iterator using the specified associative binary operator.
 The order in which operations are performed on elements is unspecified and may be nondeterministic.
 */

  Tuple2
  def reduceThings[T](list:List[T])(implicit semiGroup:Semigroup[T]) = {
    list.reduce(semiGroup combine)

  }

// TODO : -> These are specific APIS over here we dont require combine function
//todo : binary operator
val fx : (Int,Int) => Int = (x,y) => x+ y

  def   reduceInts(list:List[Int]) = list.reduce(fx)

  def   reduceStrings(list:List[String]) = list.reduce(_ + _)

  def sum (x:Int,y:Int): Int= x+y

  List(1,2,3,4).reduce(sum)

    print(reduceInts(List(1,2,3,4)))

  // TODO : -> by using Reduce things API

  val numberList: List[Int] = (1 to 4).toList

  println(reduceThings[Int](numberList))

  // TODO : -> converting List[Int] to List[Option[Int]]

  val numberOptions: List[Option[Int]] = numberList.map(Option(_))

  // TODO :-> importing the implicit  Semigroup type class instance of Type Option[Int]
  // TODO it is an higher kinded type so we will
  //  use type class instance of monoid with semigroup

  /*
  implicit def catsKernelStdMonoidForOption[A: Semigroup]: Monoid[Option[A]] =
    new OptionMonoid[A]

  TODO  : because Semigroup is also Monoid  Semigroup -> Monoid
  TODO and we have passed an implicit argument of Semigroup type Class for actual combining
   TODO
    implicit  object OptionMonoid[A](implicit A: Semigroup[A]) extends Monoid[Option[A]] {
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
}
   */
  import cats.instances.option._
  // TODO : -> it will sum all elements of List  and wrap them in Monad Option
  val result: Option[Int] = reduceThings[Option[Int]](numberOptions)
  println(reduceThings(numberOptions).getOrElse(23))

val strings= List("I am" , "starting" ,"to like " , "SemiGroup")

  val stringOption: List[Option[String]] = strings.map(Option(_))
  println(reduceThings(stringOption).getOrElse("default-string"))// It will concat the list and wrap them Option Monad
  //TODO --------------------------------------------------------------------------------------

  // TODO 1: Exercise make this api to support for Custom Types


  case class Expense(id:Long, amount:Double)
  // TODO : creating the  Type class Instance for Expense Type which is custom Type Expense
  // TODO and marking it implicit
  // TODO here as we can see we have given a reducing function so that combine will use
  implicit val typeClassInstanceExpense: Semigroup[Expense] =
  Semigroup.instance[Expense]{
    (e1,e2) => Expense(Math.max(e1.id,e2.id), e1.amount + e2.amount)
  }

  val expenses= List(Expense(1,60),Expense(2,70), Expense(3,80), Expense(4,90))

  println(reduceThings[Expense](expenses))

  //TODO : -> USe of type Enrichment or Extension Methods from Semi Group , or pimping
  // lets import the correct package
  import cats.implicits.catsSyntaxSemigroup
  /*
  TODO : type Enrichment by this implicit def catsSyntaxSemigroup
   TODO : It will inject the desired implicit type class instance of type Class SemiGroup
 TODO : i.e 2 will be converted into val enrichment= SemigroupOps(implicit typeclassinstance)(2)
  TODO : then  compiler will call enrichment.|+|(3), hence it is implicitly enriched
  trait SemigroupSyntax {

  implicit final def catsSyntaxSemigroup[A: Semigroup](a: A): SemigroupOps[A] =
    new SemigroupOps[A](a)
}

//TODO type Enrichment using implicits
implicit class SemigroupOps[A: Semigroup](lhs: A) {
  def |+|(rhs: A): A = Semigroup[A].combine(lhs, rhs)
  def combine(rhs: A): A = Semigroup[A].combine(lhs, rhs)
  def combineN(rhs: Int): A = Semigroup[A].combineN(lhs, rhs)
}
   */
  val anIntsumUsingTypeEnrichMent = 2 |+| 3

  val anStringConcatUsingTypeEnrichMent = "JAI" |+| "Mata"

  println(anIntsumUsingTypeEnrichMent)
  println(anStringConcatUsingTypeEnrichMent)
  // wen have defined typed class instance above typeClassInstanceExpense
  val aCombinedExpense: Expense = Expense(1,60) |+| Expense(2,40)
  println(aCombinedExpense)

  // TODO: implement reduce things API with |+| i.e via type Enrichment
  //TODO : here we passed Lambda combiner function fx : (x,y) => x |+| y
  //TODO : here x is implcitly converted to  SemigroupOps[instance: Semigroup](x).|+|(y)
  //TODO and that will turn into instance.combine(x,y)
  //TODO on top of that refrence we will call |+| method

  def reduceThings1[T](list:List[T])(implicit semiGroup:Semigroup[T]) = {
    list.reduce(_ |+| _)
  }

println(reduceThings1(expenses))

}
