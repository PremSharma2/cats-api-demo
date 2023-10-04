package catz

import cats.{Eval, Monoid}


object FoldingTypeClass  {

  object ListFoldingExercise{
    def map[A,B](listA:List[A])(f: A=>B):List[B]={
       listA.foldRight(List.empty[B])((currentElement, accumlatorList) =>
         f(currentElement):: accumlatorList )
    }

    def flatMap[A,B](list:List[A])(f: A=>List[B]):List[B]={
      list.foldLeft(List.empty[B])(
        (accumulator, currentElement) => accumulator ++ f(currentElement))
    }

    def filter[A](list:List[A])(predicate: A=>Boolean):List[A]={
  list.foldRight(List.empty[A])(
    (currentElement, accumlatorList)=>
      if (predicate(currentElement)) currentElement :: accumlatorList else  accumlatorList)
    }
    def combineAll[A](list:List[A])(implicit monoid:Monoid[A]): A ={
    list.foldLeft(monoid.empty)(monoid.combine)
    }
  }

// TODO Cats also Provide type classes for this kind of operation called Foldable
  /*
  TODO
   trait Foldable[F[_]]{
    def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B
    }
   */
   import cats.Foldable
   import cats.instances.list._
   import cats.instances.option._ // implicit Foldable[Option]
  val foldabletypeclassinstance: Foldable[List] =Foldable.apply[List]

  /*

      def foldLeft[A, B](fa: List[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)
   */
//def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B
  val foldedResult: Int = foldabletypeclassinstance.
    foldLeft(List(1,2,3),0)(_+_) //_+_ = (a,b) => a + b

  val foldabletypeclassinstanceforOption: Foldable[Option] =Foldable.apply[Option]

  val foldedResultOption: Int = foldabletypeclassinstanceforOption.
    foldLeft((Some(2)),0)(_+_) //_+_ = (a,b) => a + b

  // Here foldLeft is stack-safe regardless your Container
  // because we have use chained Eval which makes stake safe
  /*
   * TODO
       Transform an Eval[A] into an Eval[B] given the transformation function f.
       This call is stack-safe -- many .map calls may be chained
       without consumed additional stack during evaluation.
       Computation performed in f is always lazy,
      * even when called on an eager (Now) instance.
      * when u call sumFoldRight.value then only this get evaluated
      *
      * FlatMap(Now(0), accumulator1 =>
        FlatMap(Now(accumulator1 + 1), accumulator2 =>
        FlatMap(Now(accumulator2 + 2), accumulator3 =>
        Now(accumulator3 + 3)
    )
  )
)

   */

  val sumFoldRight: Eval[Int] = foldabletypeclassinstance.
    foldLeft(List(1,2,3),Eval.now(0)){
      (eval,num) => eval.map(_+num)
    }


  import cats.implicits.catsKernelStdGroupForInt
  import cats.instances.string._

  /*
TODO
  def combineAll[A: Monoid](fa: F[A]): A = fold(fa)
  def fold[A](fa: F[A])(implicit A: Monoid[A]): A =
    foldLeft(fa, A.empty) { (acc, a) =>
      A.combine(acc, a)
    }
   */
  // it will sum all elements of list
  val anotherSum: Int =foldabletypeclassinstance.combineAll(List(1,2,3))

  /*
TODO
  def foldMap[A, B](fa: F[A])(f: A => B)(implicit B: Monoid[B]): B =
    foldLeft(fa, B.empty)((b, a) => B.combine(b, f(a)))
   */

  val mappedConcat: String = foldabletypeclassinstance.foldMap( List(1,2,3))(_.toString)

  //TODO : working with nested data structure
  import cats.instances.vector._
  //TODO Working with nested datastructures
  val nestedDataStructure = List(Vector(1,2,3),Vector(4,5,6))
  // Here we can combine two foldables type class instances
 val foldedDatastructure= Foldable[List] compose cats.Foldable[Vector]

  val combineValue: Int = foldedDatastructure.combineAll(nestedDataStructure)
   def main(args: Array[String]): Unit = {
     import ListFoldingExercise._//Monoid[Int]
     val list = (1 to 4).toList
     println(map(list)(_+1))
     println(flatMap(list)(x=> List(x,x+1)))
     println(filter(list)(_ %2 == 0))
     println(combineValue)
    /// println(combineAll(list))
  }

}
