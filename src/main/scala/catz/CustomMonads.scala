package catz

import scala.annotation.tailrec

object CustomMonads  extends App {
// TODO Lets import the Monad Type Class
  import cats.Monad

  // TODO Lets create the implicit type-class instance for Option
  implicit object MonadOption extends Monad[Option]{
    override def pure[A](x: A): Option[A] = Option(x)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    // TODO
 //     this is for Sequential iteration of
 //      immutable data structures  with  imperative programming
    //   i.e this method is to support the impertive programming fetaures
    //   tailRec is also Looping function until i will get Right(v) i.e
    //   loop through the data structure
    //   because function types of f is A => Option[Either[A, B] so we are looking for Right[B]
    //   this is kind of looping function in scala which runs f over and over again until we gets
    //  Right
    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      case None => None
      case Some(Left(v)) =>   tailRecM(v)(f)
      case Some(Right(value)) => Some(value)
    }
  }

  //This will return the final value after
  // the iteration over this monad
  // All these iteration functions take inference of tailRecM
  // MonadOption.iterateUntil()

  // Todo Exercise Define a monad for Identity Type
// type alias
  type Identity[T] = T
  // its like type Identity = T
  val aNumber:Identity[Int] = 42


  // TODO type class instance for type Identity[T]
  implicit object IdentityMonad extends Monad[Identity]{

    override def pure[A](x: A): Identity[A] = x

    override def flatMap[A, B](a: Identity[A])(f: A => Identity[B]): Identity[B] = f(a)
     @tailrec
    override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match {
      case Left(v)=>   tailRecM(v)(f)
      case Right(value) => value
    }
  }

  // TODO : Exercise Define a Monad for Binary tree
// TODO Tree hierarchy or Faimly

  /*
   Binary Tree Reresentation
       1
      / \
     /   \
    /     \
   /       \
   2        3

TODO Here transformation will be applied over leaf nodes of Tree
   */
 sealed trait Tree[+T]
 final  case class Leaf[+T](value : T) extends Tree[T]
  //TODO: leaf is node of tree which has no branch
  //TODO : Branch is also an Tree but it has further branches or leaf nodes only
  final case class Branch [+T](left:Tree[T], right:Tree[T]) extends Tree[T]

  // TODO : We will create smart constructor instead of using case class constructor

  object Tree{
    def leaf[T](value:T): Tree[T] = Leaf(value)
    def branch[T](left:Tree[T], right:Tree[T]): Tree[T] =
      Branch(left,right)
  }

implicit object TreeMonad extends Monad[Tree] {

  override def pure[A](x: A): Tree[A] = Tree.leaf(x)

  override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = {
    fa match {
      case Leaf(v) => f(v)
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
    }
  }

  override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {

    def stackRecursive(tree: Tree[Either[A, B]]): Tree[B] = {
      tree match {
        case Leaf(Left(undesirableValue)) => stackRecursive(f(undesirableValue))
        case Leaf(Right(desirableValue)) => Leaf(desirableValue)
        case Branch(left, right) => Branch(stackRecursive(left), stackRecursive(right))
      }
    }

    stackRecursive(f(a))
  }
}
   // stackRecursive(f(a))

    /*
    TODO Here f(a) returns a Tree like this R2 ,R3 , R4 are right nodes
     3  branch has further branches which have  only right nodes available no left nodes
     where as branch 2 has further branch with one of them with left and other one with right node available
     * B4 is transformed value of Leaf

      Binary Tree Reresentation
       1
      / \
     /   \
    /     \
   /       \
   2        3
  / \      /  \
 /   \    /    \
 L1   R2  R3    R4

   tailRec([1] ,[] , [])

   //tailRec(right :: left :: todo, processed + node , doneAccumlator) this logic will run
   tailRec([3,2,1] , [1]),[])

    //tailRec(right :: left :: todo, processed + node , doneAccumlator) this logic will run
   tailRec([R4,R3,3,2,1] , [1,3]),[])

   case Leaf(Right(desirableValue)) => tailRec(todo.tail , processed , Leaf(desirableValue)::doneAccumlator) now this logic runs
   tailRec([R3,3,2,1] , [1,3]),[B4*])


   case Leaf(Right(desirableValue)) => tailRec(todo.tail , processed , Leaf(desirableValue)::doneAccumlator) now this logic runs
   tailRec([3,2,1] , [1,3]),[B3,B4])

else{
            val newLeft= doneAccumlator.head
            val newRight = doneAccumlator.tail.head
            val newBranch = Branch(newLeft,newRight)
            tailRec(todo.tail , processed , newBranch :: doneAccumlator.drop(2))
          } this will run this time
          created a new branch B34 containing the leaf nodes B3 and B4
tailRec([2,1] , [1,3]),[B34])

now  if(! processed.contains(node )){
            tailRec(right :: left :: todo, processed + node , doneAccumlator) this logic will run because 2 has not been expanded

     tailRec([R2,L1,2,1] , [1,2,3],[B34])

     now R2 is leaf then  case Leaf(Right(desirableValue)) => tailRec(todo.tail , processed , Leaf(desirableValue)::doneAccumlator)

    tailRec([L1,2,1] , [1,2,3],[B2,B34])

    now L1 is Left value  case Leaf(Left(undesirableValue)) => tailRec(f(undesirableValue) :: todo.tail , processed , doneAccumlator)
 after we apply f(a) then let say it will return a RightValue

tailRec([R1,2,1] , [1,2,3],[B2,B34])

now R1 is right value then this logic will run
case Leaf(Right(desirableValue)) => tailRec(todo.tail , processed , Leaf(desirableValue)::doneAccumlator)

    tailRec([2,1] , [1,2,3],[B1,B2,B34])

    now 2 is Branch node already expanded  then this logic will run

    val newLeft= doneAccumlator.head
            val newRight = doneAccumlator.tail.head
            val newBranch = Branch(newLeft,newRight)
            tailRec(todo.tail , processed , newBranch :: doneAccumlator.drop(2))

          tailRec([1] , [1,2,3],[B12,B34])

          now 1 again is already expanded then same Logic will run

 val newLeft= doneAccumlator.head
            val newRight = doneAccumlator.tail.head
            val newBranch = Branch(newLeft,newRight)
            tailRec(todo.tail , processed , newBranch :: doneAccumlator.drop(2))

            tailRec([] , [1,2,3],[B1234])

     */
    /*
    def tailRec[A, B](todo: List[Tree[Either[A, B]]], processed: Set[Tree[Either[A, B]]], doneAccumlator: List[Tree[B]]): Tree[B] = {
      // Lets check if this  nodes is spawned  spawned
      if (todo.isEmpty) doneAccumlator.head
      // if not then start spawning the nodes of Tree
      else todo.head match {
        case Leaf(Left(undesirableValue)) => tailRec(f(undesirableValue) :: todo.tail , processed , doneAccumlator)
          // skip the head because it is been processed and add the Leaf to done list
        case Leaf(Right(desirableValue)) => tailRec(todo.tail , processed , Leaf(desirableValue)::doneAccumlator)
        case node @  Branch(left, right) =>
          if(! processed.contains(node )){
            tailRec(right :: left :: todo, processed + node , doneAccumlator)
          }else{
            val newLeft= doneAccumlator.head
            val newRight = doneAccumlator.tail.head
            val newBranch = Branch(newLeft,newRight)
            tailRec(todo.tail , processed , newBranch :: doneAccumlator.drop(2))
          }
      }
    }
    tailRec(List(f(a)) , Set() , List())
  }
}

     */

  val tree: Tree[Int] = Branch(Leaf(2),Leaf(4))
  val transformedTree = TreeMonad.flatMap(tree)(v => Branch(Leaf(v+1),Leaf(v+2)))
  println(tree)
  println(transformedTree)

}
