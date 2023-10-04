package catz.datamanipulation

import cats.Eval

object EvaluationExercise {
  /*
TODO
 Chaining lazy computations
  One of the most useful applications of Eval is its ability to chain together computations
  in a stack-safe way. You can see one such usage when looking at the foldRight method found in Foldable.
   Another great example are mutual tail-recursive calls:
   */
  object MutualRecursion {
    def even(n: Int): Eval[Boolean] =
      Eval.always(n == 0).flatMap {
        case true => Eval.True
        case false => odd(n - 1)
      }

    def odd(n: Int): Eval[Boolean] =
      Eval.always(n == 0).flatMap {
        case true => Eval.False
        case false => even(n - 1)
      }
  }

/*
Eval.defer(
  Eval.defer(
    Eval.defer(
      Eval.now(1)
    ).map(_ * 2)
  ).map(_ * 3)
)

 */
  def factorial(n: BigInt): Eval[BigInt] =
    if(n == 1) Eval.now(1)
    else Eval.defer(factorial(n - 1).map(_ * n))

  /*
  TODO
    stack-safe lazy computation
    One useful feature of Eval is that it supports stack-safe lazy computation
    via map and flatMap methods, which use an internal trampoline to avoid stack overflow.
   You can also defer a computation which produces Eval[A] value using Eval.defer.
   Here’s how foldRight is implemented for List for example:
   */

  def foldRight[A, B](fa: List[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
    def loop(as: List[A]): Eval[B] =
      as match {
        case Nil => lb
        case h :: t => f(h, Eval.defer(loop(t)))
      }
    Eval.defer(loop(fa))
  }
  // Let’s try blowing up the stack on purpose:


  object OddEven0 {
    def odd(n: Int): String = even(n - 1)
    def even(n: Int): String = if (n <= 0) "done" else odd(n - 1)
  }
  def main(args: Array[String]): Unit = {
    MutualRecursion.odd(199999).value
    factorial(3).value
  }
}
