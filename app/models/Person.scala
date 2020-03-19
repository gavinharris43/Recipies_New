package models

import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.OFormat


case class Person(name: String, age: Int, username: String, password: String) {
  override def toString: _root_.java.lang.String = {
    s"Full Name: $name Age: $age Username: $username"
  }
}

object Search{

  val accountSearchUsername: Form[Search] =Form(
    mapping(
      "username" -> nonEmptyText
    )(Search.apply)(Search.unapply)
  )

}

object Person{

  val accountCreation: Form[Person] = Form(
    mapping(
      "name" -> nonEmptyText,
      "age" -> number(min =0 , max=120),
      "username" -> nonEmptyText,
      "password"-> nonEmptyText

    )(Person.apply)(Person.unapply)
  )


  //  def checkIfUserIsValid(userDetails: LoginDetails) = userList.contains(userDetails)
  //
  //  def getUsername(username: String) = userList.filter(user => user.username == username).headOption
}
object PersonJsonFormats {

  import play.api.libs.json.Json

  implicit val userFormat: OFormat[Person] = Json.format[Person]
}

case class Search(username: String)


