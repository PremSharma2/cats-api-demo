package catz

object FunctorsIntroduction extends App {

  /*
  TODO : The Bag along with me/map is called as Functor.
    Basically the Bag is known as a Functor when map is around

   TODO : Bag — is a Functor
        map — is a map function on Functor
        half — is a function which makes sugar half
        sugar/peanut — is a generic type A or a type of the content inside Functor

     TODO   Functors
          When a value is wrapped in a context or Bag , you can't apply a normal function to it:
           This is where fmap comes in. fmap is from the street, fmap is hip to contexts.
            fmap knows how to apply functions to values that are wrapped in a context.
            For example, suppose you want to apply (+3) to Just 2. Use fmap:

TODO:
    Functor is any data type that defines
     how fmap applies to it. Here's how fmap works:

   */
  //TODO Example of Functor
  case class Bag[A](content: A) {
    def map[B](f: A => B): Bag[B] = Bag(f(content))
  }

  // TODO : Its a Bag of sugar here
  // TODO: bags can List,Option ,Try,Either
  case class Sugar(weight: Double)
  // the guy who is expert at making sugar half
  def half = (sugar: Sugar) => Sugar(sugar.weight / 2)
  val sugarBag: Bag[Sugar] = Bag.apply[Sugar](Sugar(1)) //Bag functor of type sugar
  // map is the guy in our story who can perform operations
  // by unwrapping the bag and then calling respective function
  // and wraps the content back in a new bag
  val halfSugarBag: Bag[Sugar] = sugarBag.map(sugar => half(sugar))


  /*
  TODO: Where is unit in above code. Unit in above code is an apply method
    provided by case class and hence is not visible.
    Bag.apply[Sugar](Sugar(1)) or we can say it is pure
    Apply method takes content and returns a bag of content.
   */
}
