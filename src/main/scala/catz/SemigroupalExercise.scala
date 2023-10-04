package catz

object SemigroupalExercise {
/*
TODO
     Scenario: Product Recommendations and User Reviews
   Imagine you are building a feature for an eCommerce
   site that, when a user views a product, needs to simultaneously:

TODO
  Fetch recommended products based on the current product being viewed.
  Fetch user reviews for the current product.
   Let's assume each of these operations returns a Future (or some asynchronous context) of a list of products or reviews, respectively.

TODO
    In a monadic context like flatMap,
    you'd perform one operation after the other.
    However, there's no need for a sequential relationship here;
    fetching recommendations doesn't depend on fetching reviews and vice-versa.

TODO
    With Semigroupal,
    you can combine these two independent Futures without sequencing them.
 */

  import cats.Semigroupal
  import cats.instances.future._
  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global

  // Dummy types for demonstration
  type Product = String
  type Review = String

  def fetchRecommendations(product: Product): Future[List[Product]] = Future {
    // ... fetch recommendations from DB...
    List("Recommended Product 1", "Recommended Product 2")
  }

  def fetchReviews(product: Product): Future[List[Review]] = Future {
    // ... fetch reviews from DB...
    List("Review 1", "Review 2")
  }

  // Combine the two futures using Semigroupal
  val combined: Future[(List[Product], List[Review])] =
    Semigroupal[Future]
      .product(fetchRecommendations("Current Product"), fetchReviews("Current Product"))

}
