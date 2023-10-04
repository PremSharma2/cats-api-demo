package catz

/**
TODO
     Scenario:
     When a user places an order on an eCommerce platform,
     several steps are involved in the processing of that order,
     such as payment processing, inventory check, shipment creation, etc.
     Any of these steps can fail for various reasons.
 */
object MonadErrorExercise {

  /**
   TODO
        Assuming we represent our computations using Either
   (where Left represents an error and Right represents a success):
   */
  type Result[A] = Either[OrderError, A]

  //Error ADts
  sealed trait OrderError
  case object PaymentFailed extends OrderError
  case object OutOfStock extends OrderError
  case object ShipmentFailed extends OrderError
  // ... other potential errors

// Busness Adts

  case class Order(product: Product, quantity: Int, paymentDetails: PaymentDetails)
  case class Product(name: String, stock: Int)
  case class PaymentDetails(cardNumber: String, expiryDate: String)
  case class PaymentConfirmation(transactionId: String)
  case class InventoryStatus(product: Product, isAvailable: Boolean)
  case class Shipment(order: Order, trackingNumber: String)
  import cats.MonadError
  import cats.instances.either._


  import cats.MonadError
  import cats.instances.either._

  val monadError = MonadError[Result, OrderError]

  def processPayment(order: Order): Result[PaymentConfirmation] = {
    // Dummy implementation
    if (order.paymentDetails.cardNumber.startsWith("1234")) {
      Right(PaymentConfirmation("transaction12345"))
    } else {
      monadError.raiseError(PaymentFailed)
    }
  }

  def checkInventory(product: Product, quantity: Int): Result[InventoryStatus] = {
    // Dummy implementation
    if (product.stock >= quantity) {
      Right(InventoryStatus(product, isAvailable = true))
    } else {
      monadError.raiseError(OutOfStock)
    }
  }

  def createShipment(order: Order): Result[Shipment] = {
    // Dummy implementation
    if (order.product.name != "InvalidProduct") {
      Right(Shipment(order, trackingNumber = "tracking12345"))
    } else {
      monadError.raiseError(ShipmentFailed)
    }
  }


  def processOrder(order: Order): Result[Shipment] = {
    for {
      payment <- processPayment(order)
      inventory <- checkInventory(order.product, order.quantity)
      shipment <- createShipment(order)
    } yield shipment
  }

  // Example usage:

  val product = Product("Laptop", stock = 5)
  val paymentDetails = PaymentDetails("1234567890", "12/24")
  val order = Order(product, 1, paymentDetails)

  val result: Result[Shipment] = processOrder(order)
  println(result)  // Based on our dummy logic, this will likely print a successful shipment.

}
