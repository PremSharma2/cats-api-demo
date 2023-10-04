package catz

// TODO : -> In built type classes provided by cats library
object CatsIntro extends App {

// TODO : -> this is Eq type class here working behind the scene
  val aComparison: Boolean = "2" == "aString"
  //TODO : -> Steps to use type Class Api of cats
// TODO : -> Step    lets import the type class
  // TODO : ->  import the type class  Eq these type classes are scala in built
  // TODO : -> import cats.Eq  : -. Type class

  /*
  TODO
    Eq type class looks like this:
    trait Eq[A]{
     def eqv(x: A, y: A): Boolean
     }
   */


  // TODO : -----------------------------------------------------------------------

  // TODO : -> Step 2:  import implicit  type class instances
  //  TODO : -> for a particular type i.e Int here
// TODO : ---------------------------------------------------------------------------

// TODO : -> Step  3 use the type class API
  /*
   TODO : -> Here Eq companion object looks like this

  TODO : it takes type class instance as implicit value Like we did in
   TODO : -> Json Serialization way
  and this apply returns the type class instance
   */

  /*
  TODO : ->
   TODO : -> Companion object looks like this it will return type class instance
 TODO : ->  This is ours and below one is scala one
   object Equal{
    // this takes an type class instance in scope
    // we ca read like this use this equalizer on these values of Type T
    def apply[T](a:T,b:T)(implicit equalizer:Equal[T]) = equalizer
  }
  // This is Type Class instance this wil be automatically injected by compiler into companion apply
  implicit object    User   extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.name==b.name
   */
  /*
  TODO Cats API Companion object implementation
    object Eq extends EqFunctions[Eq] with EqToEquivConversion {
   final def apply[A](implicit ev: Eq[A]): Eq[A] = ev
   */

// TODO : -> this will automatically inject implicit type class instance
  //TODO : ->  of type int into the Eq companion because we have given the type is Int
  import cats.Eq
  import cats.instances.int._
  /*
  implicit val catsKernelStdOrderForInt: Order[Int] with Hash[Int] with LowerBounded[Int] with UpperBounded[Int] = new IntOrder
  catsKernelStdOrderForInt is an instance of a type that
  implements the Order, Hash, LowerBounded,
  and UpperBounded type classes for the Int type.
   */

  // Here we are calling Eq type class companion object which in turn returns the
  // TODO : Type class instance
  val intEqualityTypeClassInstance: Eq[Int] = Eq.apply[Int]
  /*
  TODO
    now we will call eqv method of type class Eq via type class instance intEqualityTypeClassInstance
    def eqv[@sp Int](x: Int, y: Int)(implicit ev: E[Int]): Boolean =
    ev.eqv(x, y)

   */
  // TODO : calling eqv def over typeclass instance
   val eqResult: Boolean =intEqualityTypeClassInstance.eqv(2,2)



  /*
  Like this : ->
//TODO : This example making  us understand that how scala Work internally
  trait Equal[T]{
    def eqv(a:T,b:T):Boolean
  }
  //Companion object for equal
  object Equal{
    // this takes an type class instance
    // we ca read like this use this equalizer on these values of Type T
    def apply[T](a:T,b:T)(implicit equalizer:Equal[T]) :Boolean= equalizer.apply(a,b)
  }
  // This is Type Class instance
  implicit object NameEquality extends Equal[User] {
    override def eqv(a: User, b: User): Boolean = a.name==b.name
   */
// TODO : Equal[USer].apply.eqv(a,b)

  //--------------------------------------------------------------
  // TODO :  Step4 for Impl
  //  use extension methods or type enrichment by importing it
  /*
  Like this : ->

 TODO: -> type Enrichment or extension method
  implicit class TypeSafeEqual[T](value:T){
    def ===(other:T)(implicit equalizer:Equal[T]) = equalizer.apply(value,other)
    def =!=(other:T)(implicit equalizer:Equal[T]) = !equalizer.apply(value,other)
  }
  Note: But cats API uses implicits defs and implicits vals and we are using
  implicits classes

   */
  import cats.syntax.eq._
  val a: Boolean = 2===3

  // TODO : -> part 5 Extending type class operation to composite types
  // TODO : -> for that we need to import the correct implicit  type  class instance
  import cats.instances.list._ // by this we bring Eq[List[Int]]
  val aListcomparison: Boolean = List(2) === List(3)

// TODO -----------------------------------------------------------------------------------

  // TODO : Exercise  create a type class instance for custom type
  case class ToyCar(model :String, price:Double)
  // create your own type class using cats API
  // TODO : now what scala will  do here it will create a type class instance
  //TODO :i.e Eq[A].instance(f)(implicit typeclass object) def
  // but it is not singleton object
  // it is anonymous class object
  // using this function f: (A, A) => Boolean type class will compare your custom objects
  //i mean its an way of creating type class instance for custom type
  // with this method scala will give you type class instance
  //and we marked it implicit so that this type class instance cam be marked implicit
  /*
  TODO: instance method implementation
    def instance[A](f: (A, A) => Boolean): Eq[A] =
    new Eq[A] {
      def eqv(x: A, y: A) = f(x, y)
    }
   */

  implicit val toyCar: Eq[ToyCar] = Eq.instance[ToyCar]{
    (car1,car2) => car1.price == car2.price
  }


  val toyCarrs: Boolean =toyCar.eqv(ToyCar("ferari", 29.99), ToyCar("ferari", 29.99))
  val compareToyCars= ToyCar("ferari", 29.99) === ToyCar("Baleno" , 39.99)
  println(compareToyCars)
}
