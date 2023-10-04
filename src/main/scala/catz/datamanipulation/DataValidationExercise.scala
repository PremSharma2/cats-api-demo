package catz.datamanipulation

import java.util.UUID

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}

import scala.util.Try

object DataValidationExercise extends App {

  case class UserInfo(email: String, phoneNumber: String)
  case class Shipping(address: String, city: String, zip: String)
  case class Payment(cardNumber: String, expiration: String, cvv: String)

  /*
  TODO
        Cats provides an extension type of the main Validated type,
         that we are going to use in our example:
       type ValidatedNel[+E, +A] = Validated[NonEmptyList[E], A]
        The cats.data.ValidatedNel models the error accumulation use case,
        that it is exactly what we need.
   */

  import cats.data._
  import cats.implicits._

  /**
  Use ValidatedNec (which stands for "Validated Non-empty Chain") to accumulate validation errors.
   */
  object Validators {
    type ValidationResult[A] = ValidatedNec[String, A]

    def validateEmail(email: String): ValidationResult[String] =
      if (email.contains("@")) email.validNec else "Invalid email format".invalidNec

    def validatePhoneNumber(phone: String): ValidationResult[String] =
      if (phone.matches("[0-9]{10}")) phone.validNec else "Invalid phone number".invalidNec

    // ... Additional validators for address, cardNumber, etc.

    def validateUserInfo(userInfo: UserInfo): ValidationResult[UserInfo] =
      (validateEmail(userInfo.email), validatePhoneNumber(userInfo.phoneNumber)).mapN(UserInfo)

    // ... Additional combined validators for Shipping and Payment
  }


}


