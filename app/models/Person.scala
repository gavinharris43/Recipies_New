package models

import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.OFormat
import reactivemongo.bson.BSONObjectID

case class Search(username: String)
case class UpdatePassword(username: String, password: String, UpdatePassword: String)
case class Person(_id: BSONObjectID, name: String, age: Int, username: String, password: String) {
  val loginDetails: LoginDetails = LoginDetails(username, password)
  val id: BSONObjectID = _id

  override def toString: _root_.java.lang.String = {
    s"ID: ${id.stringify} Full Name: $name Age: $age Username: $username"
  }
}

object Search {

  val accountSearchUsername: Form[Search] = Form(
    mapping(
      "username" -> nonEmptyText
    )(Search.apply)(Search.unapply)
  )

}

object UpdatePassword {

  val accountUpdatePassword: Form[UpdatePassword] = Form(
    mapping(
      "username" -> nonEmptyText,
      "password" -> nonEmptyText,
      "newPassword" -> nonEmptyText
    )(UpdatePassword.apply)(UpdatePassword.unapply)
  )

}

object Person {

  def apply(name: String, age: Int, username: String, password: String) = new Person(BSONObjectID.generate(), name, age, username, password)

  def unapply(arg: Person): Option[(BSONObjectID, String, Int, String, String)] = Option((arg._id, arg.name, arg.age, arg.username, arg.password))

  val accountCreation: Form[Person] = Form(
    mapping(
      "id" -> ignored(BSONObjectID.generate: BSONObjectID),
      "name" -> nonEmptyText,
      "age" -> number(min = 0, max = 120),
      "username" -> nonEmptyText,
      "password" -> nonEmptyText

    )(Person.apply)
    (Person.unapply)
  )

  val accountDeletion: Form[LoginDetails] = Form(
    mapping(
      "username" -> nonEmptyText,
      "password" -> nonEmptyText

    )(LoginDetails.apply)
    (LoginDetails.unapply)
  )
}

object JsonFormats {

  import play.api.libs.json.Json

  implicit val bsonObjectIDFormat: OFormat[BSONObjectID] = Json.format[BSONObjectID]
  implicit val personFormat: OFormat[Person] = Json.format[Person]
  implicit val loginDetailsFormat: OFormat[LoginDetails] = Json.format[LoginDetails]
}



