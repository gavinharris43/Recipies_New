package controllers


import authentication.AuthenticationAction
import javax.inject.{Inject, Singleton}
import models.JsonFormats._
import models.{LoginDetails, Person}
import play.api.libs.json.{JsObject, Json}
import play.api.mvc._
import play.modules.reactivemongo.{MongoController, ReactiveMongoApi, ReactiveMongoComponents}
import reactivemongo.api.Cursor
import reactivemongo.play.json._
import service.MongoService

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

@Singleton
class LoginController @Inject()(components: ControllerComponents,
                                val reactiveMongoApi: ReactiveMongoApi,
                                val mongoServices: MongoService,
                                authAction: AuthenticationAction
                               ) extends AbstractController(components)
  with MongoController with ReactiveMongoComponents with play.api.i18n.I18nSupport {

  implicit def ec: ExecutionContext = components.executionContext

  def login(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.login(LoginDetails.loginForm))
  }

  def loginSubmit(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    LoginDetails.loginForm.bindFromRequest.fold({ formWithErrors =>
      BadRequest(views.html.login(formWithErrors))
    }, { loginDetails =>
      if (checkIfUserIsValid(loginDetails))
        Redirect(routes.HomeController.index()).withSession(request.session + ("username" -> loginDetails.username))
      else
        BadRequest(s"Incorrect username or password. $loginDetails ")
    })
  }

  def checkIfUserIsValid(details: LoginDetails): Boolean = {
    //    val personList: List[Person]=getLoginList(details).value.getOrElse(Try(List[Person]())).get
    val personList: List[Person] = Await.result(getLoginList(details.username), Duration.Inf)

    val userList: List[LoginDetails] = personList.map(el => el.loginDetails)

    userList.contains(details)

  }


  def getLoginList(username: String) = {

    mongoServices.collection.map {
      _.find(filterToJsonObj("username", username))
        .sort(Json.obj("created" -> -1))
        .cursor[Person]()
    }.flatMap(el => {
      el.collect[List](
        -1,
        Cursor.FailOnError[List[Person]]()
      )
    }
    )
  }

  def filterToJsonObj(filteredValue: String, value: String): JsObject = {
    Json.obj((filteredValue, Json.toJsFieldJsValueWrapper(value)))
  }

  def getUsername(username: String): Option[LoginDetails] = {
    val personList: List[Person] = Await.result(getLoginList(username), Duration.Inf)
    val userList: List[LoginDetails] = personList.map(el => el.loginDetails)
    Option(userList.head)
  }


  def logout() = authAction {
    implicit request =>
      Redirect("/").removingFromSession("username")
  }

}
