package catz

import java.time.LocalDate

object TypeAlias {


/*

TODO
    Type alias composition in Scala involves creating new types
     by combining existing types or type aliases
     to provide more meaningful names for specific use cases.
     This technique enhances code readability and expressiveness.
     Here are some real-world use cases for type alias composition in Scala:
     1. **Domain Modeling**:
   - **Use Case**: When modeling complex domains,
       you can compose type aliases to represent compound concepts.
       For example, when working on a banking application,
       you might compose types for different monetary values.
   - **Example**:
 */
  type Dollars = BigDecimal
  type Euros = BigDecimal
  type CurrencyAmount = Map[String, BigDecimal]

  /*
TODO
     2. **Configuration Parameters**:
   - **Use Case**: When dealing with configuration parameters,
     you can use type alias composition to
     create self-explanatory types for various configuration settings.
   - **Example**:
   */

  type Hostname = String
  type Port = Int
  type DatabaseConfig = (Hostname, Port, String, String)

  /*
TODO
    3. **Units of Measurement**:
   - **Use Case**:
      Type aliases can be used to represent units
     of measurement, making it clear what a value represents in terms of units.
   - **Example**:
   */


  type Kilograms = Double
  type Meters = Double
  type Weight = Kilograms
  type Length = Meters


  /*
TODO
    4. **Error Handling**:
   - **Use Case**: Type alias composition can be valuable when defining custom error types or error handling strategies.
   - **Example**:
   */

  type ValidationError = String
  type Result[A] = Either[ValidationError, A]

/*
TODO
  **API Responses**:
    - **Use Case**:
    *  When working with APIs,
    *  you can compose type aliases to represent
    * the structure of API responses, making the code more self-documenting.
    - **Example**:


 */
  type ApiResponse[A] = Either[ApiError, A]
  type ApiError = String

/*
TODO
  6. **Caching Keys**:
  - **Use Case**:
   In caching scenarios,
   type alias composition can be used to create descriptive keys for caching data.
  - **Example**:


 */
  type CacheKey[A] = (String, A)

/*
TODO
  **User Roles and Permissions**:
    - **Use Case**:
    * When working on a system with user roles and permissions,
    * you can use type aliases to represent roles and permissions more clearly.
    - **Example**:


 */
  type Role = String
  type Permission = String
  type UserRoles = Set[Role]
  type UserPermissions = Set[Permission]


/*
Todo
  **Logging Levels**:
    - **Use Case**:
    Type alias composition can be helpful
    in setting up logging levels for different parts of your application.
    - **Example**:

 */
  type LogLevel = String
  type AppLogLevel = LogLevel
  type DatabaseLogLevel = LogLevel


/*
TODO
  **Authentication Tokens**:
    - **Use Case**:
    When working with authentication tokens,
    type aliases can help distinguish between different token types.
    - **Example**:


 */
  type AccessToken = String
  type RefreshToken = String
  type AuthTokenPair = (AccessToken, RefreshToken)


  /**
TODO
    Suppose you are working on an investment banking system
    that deals with various financial instruments,
    including stocks and bonds. You want to represent
     these instruments with their specific properties
     and create flexible data structures for pricing and trading
TODO
     Union Types (OR):
    In this case, you can use union types to represent
    the different types of financial instruments,
    such as stocks and bonds, each with its own set of attributes.
   */

  sealed trait FinancialInstrument

  case class Stock(symbol: String, price: BigDecimal, volume: Long) extends FinancialInstrument
  case class Bond(isin: String, faceValue: BigDecimal, maturityDate: LocalDate) extends FinancialInstrument

  val stock: FinancialInstrument = Stock("AAPL", 150.0, 1000)
  val bond: FinancialInstrument = Bond("US123456789", 1000.0, LocalDate.of(2030, 12, 31))


  /*
TODO
    Intersection Types (AND):
                  You can use intersection types to create more specific
                  types that combine multiple characteristics.
                   For instance, you might want to represent a financial instrument
                   that has both equity and fixed income attributes.
   */

  trait Equity {
    def symbol: String
    def price: BigDecimal
    def volume: Long
  }

  trait FixedIncome {
    def isin: String
    def faceValue: BigDecimal
    def maturityDate: LocalDate
  }

  type EquityStock = FinancialInstrument with Equity
  type FixedIncomeBond = FinancialInstrument with FixedIncome

  val equityStock = Stock("AAPL", 150.0, 1000)
  val fixedIncomeBond = Bond("US123456789", 1000.0, LocalDate.of(2030, 12, 31))

}
