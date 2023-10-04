package catz.datamanipulation

import cats.Eval

object FunctionalState  extends App {

  //TODO In functional programming the state is Data Structure
  //TODO state is nothing but it is equivalent to iterative computations of oops
  //TODO :  the State monad allows you to model computations that read from and modify state.
    //Here S is the state we need to manage and A is the desirable value we obtain from single computation
  //The State monad is essentially a function that takes
  // some state and returns a value and a new state.
  // It's often defined as S => (S, A),
  // where S is the type of the state
  // and A is the type of the value being computed
  //val f: Int => (Int, String) = ???  // or Function1[Int, Tuple[Int, String]] so we type alisaes with MState[Int,String]
  type MStat[S,A] = Function1[S, Tuple2[S, A]]

     val f: Int => (Int, String) = (x:Int)=> (x+1,"Hello")

  type MState[Int,String] =  Int => (Int, String)  // // or Function1[Int, Tuple[Int, String]]
  /*
TODO
     MyState[S,A]:
     This introduces a new type alias called MyState
     which expects two type parameters S and A.
     S => (S,A):
     This means the actual type represented by MyState[S,A]
     is a function that takes an S and returns a tuple (S, A)
   */
  type MyState[S,A] = S => (S,A)

  val s1:MyState[Int,String] = f

  def increment(s: Int): (Int, String) = (s + 1, s"Previous: $s")
//more efficient
  val increment: MyState[Int, String] = s => (s + 1, s"Previous: $s")
  //modifing the state
increment.apply(1)



  import cats.data.State
  /*
TODO
  final class IndexedStateT[F[_], SA, SB, A](val runF: F[SA => F[(SB, A)]])
  type StateT[F[_], S, A] = IndexedStateT[F, S, S, A]
  //new MyMonad[Option]
  type State[S, A] = StateT[Eval, S, A]
  //-------------------------------------------------------------------------
TODO
 object State{
   def apply[S, A](f: SA => (SB, A)): State[SB, A] =
    IndexedStateT.applyF(Now((s: S) => Now(f(s))))
    }
  //------------------------------------------------------------
TODO
     def applyF[F[_], SA, SB, A](runF: F[SA => F[(SB, A)]]): IndexedStateT[F, SA, SB, A] =
    new IndexedStateT(runF)
TODO
//------------------------------------------------------------------------------------
    final class IndexedStateT[F[_], SA, SB, A](val runF: F[SA => F[(SB, A)]])
   */
  //TODO State is a wrapper over function which manages the state
  // the idea here is that we are changing the state via pure FP
  /*
   def apply[S, A](f: SA => (SB, A)): State[SB, A] =
    IndexedStateT.applyF(Now((s: S) => Now(f(s))))
   */
  // TODO Hence in FP the state will be managed by the functions
  // TODO so fy is a function which takes initial input as initial state S and converts it into the
  // TODO new state S1 and to play with this we need to wrap it inside some container
  val fy: Int => (Int, String) = (currentCount) => (currentCount+1,s"incremented the state")

  val countAndSay : State[Int,String]= State.apply(fy)

  val runf: Eval[Int => Eval[(Int, String)]] =countAndSay.runF // TODO getting the Eval form the State


  /*
 TODO
   runF= Now((s: S) => Now(f(s)))
  def run(initial: SA)(implicit F: FlatMap[F]): F[(SB, A)] =
  //f: (initial: S) => Now(fx(initial))
    F.flatMap(this.runF)(f => f.apply(initial))
    F.flatMap(Eval[Int => Eval[(Int, String)]])(f => f.apply(initial))

 */
  val result: Eval[(Int, String)] = countAndSay.run(10)

  //TODO : ->  Eval.value will execute this Fx : (10: Int) => Now(f(10))
val newState: (Int, String) = countAndSay.run(10).value


  println(result.value)


// changing the state in OOPs or iterative programming
  //TODO iterative computation in oops
  var a = 10
  a+=1 // state s1
  val firstComputation = s"Added 1 to 10 , obtained $a"

  a*=5  // state s2
  val secondMutation= s"Multiplied with 5 , obtained $a"

//TODO But what if we have to implement the same computation In pure FP
  // TODO As we know that state can manged by functions in FP
  //  so we have created two functions
  // TODO fx and fz both are involved in state transformation
  // so now task is to run fx and fz in chain two compute
  // the state transformation or state change
  val fx= (s:Int) => (s+1, s"Added 1 to 4, and obtained ${s+1}" )
//represents a stateful computation
  val firstTransformation: State[Int, String] = State.apply(fx)

  val fz= (s:Int) => (s*5, s"Multiplied with 5 and  obtained  ${s*5}" )

  //represents a stateful computation
  val secondTransformation: State[Int, String] = State.apply(fz)



/*
TODO
   in map scala have chained  composing two functions
 using andThen def
 i.e composed two functions
// TODO : chaining of transformation can only be dine via flatMap

//TODO Lets talk about map function first

TODO
 val fx= (s:Int) => (s+1, s"Added 1 to 4, and obtained ${s+1}" )
  val firstTransformation: State[Int, String] = State.apply(fx)
  firstTransformation.map(s=> "state-changed")

// TODO map function internals


TODO
 def map[B](f: A => B)(implicit F: Functor[F]): IndexedStateT[F, SA, SB, B] =
    transform { case (s, a) => (s, f(a)) }
TODO
    def transform[B, SC](f: (SB, A) => (SC, B))(implicit F: Functor[F]): IndexedStateT[F, SA, SC, B] =
    IndexedStateT.applyF(
       F.map(this.runF) { sfsa =>  //SA => Now((SB, A))
      AndThen(sfsa).andThen { fsa => // Now((SB, A))
        F.map(fsa) { case (s, a) => f.apply(s, a) }
      }
    })

 def applyF[F[_], SA, SB, A](runF: F[SA => F[(SB, A)]])
 */
  /*
  def run(initial: SA)(implicit F: FlatMap[F]): F[(SB, A)] =
    F.flatMap(runF)(f => f(initial))
   */
  val tansformedComputation = firstTransformation.map(a=> "state-changed")
  val transfrmedresult: Eval[(Int, String)] =tansformedComputation.run(2)


//TODO : -> composing two transformations
  // TODO here first computation will run
  // and the o/p of i.e (Int,String) will be given second transformation
  // i.e state as well as value returned by single computation as input
  // so 5 will be given to second transformation


  /*

TODO
   def flatMap[B, SC](fas: A => IndexedStateT[F, SB, SC, B])(implicit F: FlatMap[F]): IndexedStateT[F, SA, SC, B] =
    IndexedStateT.applyF(F.map(this.runF) { safsba => //  SA => Now((SB, A))
      AndThen(safsba).andThen { fsba => // Now((SB, A))
        F.flatMap(fsba) {
          case (sb, a) =>
            fas(a).run(sb)
        }
      }
    })

   */
  // TODO this is classical example of the composing two state
  //  transformations i.e o/p of one state transformation
  // TODO is the input to the other state transformation
  //TODO flatMap is used for this compose transformation

val rs: Eval[(Int, (String, String))] =  fxy.apply("firstResult").run(2)

  val fxy: String => State[ Int,(String, String)]= {
    firstResult => secondTransformation.
    map(secondResult => (firstResult,secondResult))
  }

  val fx1= (s:Int) => (s+1, s"Added 1 to 4, and obtained ${s+1}" )
  //represents a stateful computation
  val firstTransformation1: State[Int, String] = State.apply(fx)
val composedTransformation:State[Int,(String,String)] = firstTransformation.flatMap(fxy)


 val composedResult: Eval[(Int, (String, String))] = composedTransformation.run(4)
  println(composedResult.value)

  val FirstTransformation: State[Int, String] = State.
    apply((s:Int) => (s+1, s"Added 1 to 4, and obtained ${s+1}" ))

//TODO using for comprehension
  val compositeTransformation: State[Int, (String, String)] = for {
      firstResult <- FirstTransformation
      secondResult <-  secondTransformation
  } yield (firstResult,secondResult)

// TODO : we can achieve this by chaining the two functions
  // TODO and that's what scala does internally
  val func1: Int => (Int, String) = (s:Int) => (s+1, s"Added 1 to 4, and obtained ${s+1}" )
  val func2: Int => (Int, String) = (s:Int) => (s*5, s"Multiplied with 5 and  obtained  ${s*5}" )


/*
TODO
 Composes two instances of Function1 in a new Function1, with this function applied first.
 Params:
 g – a function R => A
 Type parameters:
 A – the result type of function g
 Returns:
 a new function f such that f(x) == g(apply(x))
 def andThen[A](g: R => A): T1 => A = { x => g(this.apply(x)) }
 */
  val composeFunction: Int => (String, (Int, String)) = func1.andThen{
    case (newState,firstState) => (firstState,func2(newState))
  }
  println(composeFunction.apply(2))




  //TODO Exercise Online store
  case class ShoppingCart(items:List[String], total:Double)
  type ItemName = String

  def addToCart(item:ItemName, price:Double ):State[ShoppingCart,Double] = {

    State.apply{

      existingCart => (ShoppingCart(item :: existingCart.items, existingCart.total + price),price + existingCart.total)
    }

  }

  /*
  val compositeTransformation: State[Int, (String, String)] = for {
      firstResult <- FirstTransformation
      secondResult <-  secondTransformation
  } yield (firstResult,secondResult)
   */

 val cartState: State[ShoppingCart, Double] = addToCart("Orange",30)

 val transfomrdCart: Eval[(ShoppingCart, Double)] =
   cartState.run(ShoppingCart(List.empty[String],0.0))

  val compositeCart: State[ ShoppingCart, Double] =
     addToCart("Apple",20)
    .flatMap(_ => addToCart("banana",30).
      flatMap{ _ => addToCart("ElectricOven",50)
  })
  val totalValue: State[ShoppingCart, Double] = compositeCart.map(tr=> tr)
  totalValue.run(ShoppingCart(List.empty[String],0.0))

val premcart:State[ShoppingCart,Double] = for{
  _ <- addToCart("Apple",20)
  _ <- addToCart("banana",30)
  total<- addToCart("ElectricOven",50)
} yield total

  println(premcart.run(ShoppingCart(List(),0.0)).value)
}
