package catz

import java.util.concurrent.Executors

import scala.concurrent.{ExecutionContext, Future}
/*
TODO : The bag with unit alias pure , flatMap around  makes a Monad.
       It was another shock for me. Monad!, so this is what is a Monad.
 It’s like a Functor who knows how to flatten.
  unit, map , flatten you guys are cool but wouldn’t it be convenient
  if there is someone who knows how to unwrap/wrap and flatten both.
  Someone who can do the job of map and flatten” I said.
TODO : Like this map is there because monad extends Functor
TODO
  case class Bag[A](content: A) {
    def map[B](f: A => B): Bag[B] = Bag(f(content))
    def flatMap(f: A=> Bag[B]) : Bag[B] =f(content)
  }
  So List , Future , Try all are Monads
 */
object lMonads extends App {
  val numbersList= List(1,2,3,4) // List Bag
  val charsList: Seq[Char] =List('a','b','c')


  //TODO : Execise 1 how would you create the combinations of character and numbers
  // TODO : i.e Extract transform and Wrap for that flatMap is best
  // TODO : for transform ww will use map function which convert A=> B
  // in both the List
  // TODO Solution1:
  val combinationList: List[(Int, Char)] = numbersList.
                                          flatMap(n => charsList.map(char => (n,char)))

  // TODO : Solution number 2
  val forComprehension: List[(Int, Char)] = for{
    number <- numbersList
    char <- charsList
  } yield (number,char)

  //TODO :summary both impl are identical

  //TODO : Execise 1.2 how would you create the combinations of character and numbers (n,c)
  val numberOption= Option(2)
  val charOption=Option('c')

  val combinationOption: Option[(Int, Char)] = numberOption.flatMap(n=> charOption.map(ch=> (n,ch)))
  val forComprehensionForOption: Option[(Int, Char)] = for{
    number <- numberOption
    char <- charOption
  } yield (number,char)

  //TODO : Futures
  implicit val ec: ExecutionContext = ExecutionContext.
                                      fromExecutorService(Executors.newFixedThreadPool(2))

  //TODO : Exercise 1.3 how would you create the combinations of character and numbers (n,c)
  val numberFuture= Future(42)
  val charFuture= Future('c')
//TODO : here flatmap make sure that they will make the sequential acces to these tasks
  val combinationFuture: Future[(Int, Char)] = numberFuture.

                                                     flatMap(n=> charFuture.map(ch=> (n,ch)))
  val forComprehensionForFuture = for{
    number <- numberFuture
    char <- charFuture
  } yield (number,char)


}
