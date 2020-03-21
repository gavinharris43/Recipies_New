package models

import play.api.data.Form
import play.api.data.Forms._

case class LoginDetails(val username: String, password: String)


object LoginDetails {

  val loginForm: Form[LoginDetails] = Form(
    mapping(
      "username" -> nonEmptyText,
      "password" -> nonEmptyText
    )(LoginDetails.apply)(LoginDetails.unapply)
  )




}
