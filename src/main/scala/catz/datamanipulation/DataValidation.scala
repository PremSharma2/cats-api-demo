package catz.datamanipulation

import cats.Semigroup
import cats.data.Validated

import scala.annotation.tailrec
import scala.util.Try
/*
TODO
    It acts like Either[Left,Right]  i.e if we get
    correct value we get Int and lese we get String
    :Validated[String,Int]
 */
object DataValidation  {
// import cats.data.Validated
val aValidValue:Validated[String,Int] = Validated.
  valid(42) // this will be equivalent to Right(42) Valid(42)

  val aInvalidValue: Validated[String, Int] =  Validated.
    invalid("Validation failed returning Invalid value!!")// this is equivalent to Left("Validation failed returning Invalid value!!")
  //
  //so it will return Valid alias Right or Left cond is used to to validate the Expression using Validated API !!! alias Invalid
  val aTest: Validated[String, Int] = Validated.cond(42>22 , 42 , "Validation failed!!")

  // TODO Exercise
  /*
    TODO
      We want to test if the method testNumber meets the following condition
      1 Number must be a prime
      2 number must be non negetive
      3 n<100
      4 n must be even
      5 and if n is failing all or some conditions then failure should be wrapped in a List
      i.e if it is failing for 3 conditions then list("failed","failed","failed") like that should be returned
   */

    def testPrime(n:Int)  = {
      @tailrec
      def tailRecPrime(d: Int): Boolean = {
        if (d < 1) true
        else n % d != 0 && tailRecPrime(d - 1)
      }
      // bcz 1,-1,0 neither prime nor primes
      if (n == 0 || n == 1 || n == -1) false
      else tailRecPrime(Math.abs(n / 2))
    }



  def testNumber(n:Int):Either[List[String] ,Int] ={
    val isNoteven = if (n%2 == 0) List() else List("Number is not even!!")
    val isNegetive = if (n>= 0) List()   else List("Number is negetive!!")
    val isToBig= if (n<=100) List()   else List("Number is too big!!")
    val isNotPrime = if(testPrime(n))  List()   else List("Number is not Prime!!")
    if(n%2 == 0 && n>=0 && n<=100 && testPrime(n) ) Right(n)
    else Left(isNegetive ++ isNoteven ++ isNotPrime ++ isToBig)
  }
//TODO this Exercise can be done nicely by Validation API
  import cats.instances.list._
 // implicit val combineIntMax:Semigroup[Int] = Semigroup.instance[Int](Math.max)
  /*
  def combine[EE >: E, AA >: A](that: Validated[EE, AA])(implicit EE: Semigroup[EE],
                                                         AA: Semigroup[AA]): Validated[EE, AA] =
    (this, that) match {
      case (Valid(a), Valid(b))     => Valid(AA.combine(a, b))
      case (Invalid(a), Invalid(b)) => Invalid(EE.combine(a, b))
      case (Invalid(_), _)          => this
      case _                        => that
    }

   */
  implicit val combineIntMax:Semigroup[Int] = Semigroup.instance[Int](Math.max)
  //TODO composing multiple validations
  def validateNumber(n:Int):Validated[List[String],Int] =
      Validated.cond(n%2 ==0 ,n,List("Number Must be Prime!!") ).
      combine[List[String],Int]( Validated.cond(n>= 0 ,n,List("Number is negetive!!") ))
      .combine(Validated.cond(n<=100 ,n,List("Number is too big !!") ))
      .combine(Validated.cond(testPrime(n) ,n,List("Number is not Prime!!!!") ))
      // chain of functions


  /*
  Apply a function (that returns a Validated) in the valid case.
   Otherwise return the original Validated.
This allows "chained" validation: the output of one validation can be fed into another validation function.
This function is similar to flatMap on Either.
 It's not called flatMap, because by Cats convention, flatMap is a monadic bind
 that is consistent with ap.
  This method is not consistent with ap (or other Apply-based methods),
  because it has "fail-fast" behavior as opposed to accumulating validation failures.
  def andThen[EE >: E, B](f: A => Validated[EE, B]): Validated[EE, B] =
    this match {
      case Valid(a)       => f(a)
      case i @ Invalid(_) => i
    }
   */


  val anThenResult: Validated[String, Int] =aValidValue.andThen(_=> aInvalidValue)

  // we can also test a valid value with ensure
  // if a valid value pass this new predicate then it will remain as valid
  // else it will turn into the Invalid value
 val avalidValueTest= aValidValue.ensure(List("Something Went wrong"))(_%2==0)
  // we can also transform the  Validated

  /*
TODO
    def map[B](f: A => B): Validated[E, B] = this match {
    case i @ Invalid(_) => i
    case Valid(a)       => Valid(f(a))
  }
   */

  aValidValue.map(_+1)

  // if we want to transform the undesirable value or the left value or
  // invalid value then
  aInvalidValue.leftMap(_ + "failed value")

  // we also transform both together
  aValidValue.bimap(_+"invalid value" , _+1)

  //interpolate with stdlib
  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right[List[String], Int](42))
  val optionToValidated: Validated[List[String], Int] = Validated.fromOption(Some(42),List("Its is None value"))
  val tryToValidated: Validated[Throwable, Int] = Validated.fromTry(Try("something".toInt))


  //TODO: backward compatibility

  val validatetoEither: Either[String, Int] =aValidValue.toEither
  aValidValue.toOption

  //TODO implicit conversion or type enrichment
  import cats.syntax.validated._

  val aValidwithExtesionmethods: Validated[List[String], Int] = 42.valid[List[String]]

  val anError: Validated[String, Int] ="Something".invalid[Int]


  // TODO Exercise number 2 Form validation
  object FormValidation{
    import cats.instances.string._
    type FormValidation[T] =  Validated[List[String], T]
    /*
    fields are
    name
    email
    password
    rules are name email and password must be  specified
    name must not be blank
    email must have @ character
    password must have atleast 10 characters

     */
    def validateForm(form:Map[String,String]): FormValidation[String] ={
           getValue(form,"Name").
           andThen(name=> nonBlank(name,"Name"))
          .combine(getValue(form,"Email")).andThen(emailProperForm)
          .combine(getValue(form,"Password")).andThen(passwordCheck)
          .map(_=> "User is Validated Successfully")
    }

    def getValue(form:Map[String,String], fieldname:String):FormValidation[String] ={
      Validated.fromOption(form.get(fieldname), List(s"field name $fieldname must be specified"))
    }

    def nonBlank(value:String, fieldName:String):FormValidation[String] = {
      Validated.cond(fieldName.length>0,value,List("Field must not be Empty or blank"))
    }
    def emailProperForm(email:String):FormValidation[String] ={
      Validated.cond(email.contains("@"),email,List("Email is not in proper format"))
    }
    def passwordCheck(password:String):FormValidation[String] ={
      Validated.cond(password.length>=10,password,List("passwords must be longer then 10 characters"))
    }
  }
  def main(args: Array[String]): Unit = {
    val form= Map(
      "Name" -> "Prem",
      "Email" -> "prem.kaushik@outlook.com",
      "Password" ->"Welcome"
    )

    println(FormValidation.validateForm(form))
  }

}
