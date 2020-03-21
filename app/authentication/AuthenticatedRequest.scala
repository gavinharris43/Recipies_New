package authentication


import javax.inject.Inject
import models.JsonFormats._
import models.{LoginDetails, Person}
import play.api.Logger
import play.api.libs.json.{JsObject, Json}
import play.api.mvc._
import reactivemongo.api.Cursor
import reactivemongo.play.json._
import service.MongoService

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

class AuthenticatedRequest[A](val username: String, request: Request[A]) extends WrappedRequest[A](request)

class AuthenticationAction @Inject()(val parser: BodyParsers.Default)(implicit val executionContext: ExecutionContext, val mongoService: MongoService)
  extends ActionBuilder[AuthenticatedRequest, AnyContent] {

  override def invokeBlock[A](request: Request[A], block: AuthenticatedRequest[A] => Future[Result]): Future[Result] = {
    request.session.get("username")
      .flatMap(username => getUsername(username))
      .map(user => block(new AuthenticatedRequest(user.username, request)))
      .getOrElse(Future.successful(Results.Redirect("/login")))
  }


  def getLoginList(username: String) = {

    mongoService.collection.map {
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
    val userList: List[LoginDetails] = Await.result(getLoginList(username), Duration.Inf).map(el => el.loginDetails)
    userList.headOption
    }

}
