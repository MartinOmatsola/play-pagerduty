package controllers

import play.api._
import play.api.mvc._
import play.api.db.DB
import anorm._
import play.api.Play.current
import play.api.libs.ws
import play.api.libs.ws.WS
import play.api.libs.ws.WS.WSRequestHolder
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.json._
import play.api.libs.functional.syntax._

case class Alert(
  id: String, 
  contactType: String, 
  startedAt: String,
  address: String
)

case class Incident(
  id: String,
  htmlUrl: String,
  status: String
)

object Application extends Controller {
  
  val apiKey = ""
  val endPoint = ""

  //TODO: make date range dynamic
  val dateRange = Map("since" -> "2013-04-01", "until" -> "2013-04-20")

  val alertReads = (
      (__ \ "id").read[String] ~
      (__ \ "type").read[String] ~
      (__ \ "started_at").read[String] ~
      (__ \ "address").read[String]
  )(Alert) 

  val incidentReads = (
      (__ \ "id").read[String] ~
      (__ \ "html_url").read[String] ~
      (__ \ "status").read[String] 
  )(Incident) 

  def index = getAlerts

  def getAlerts() = Action {
    Async {
        getRequest("alerts", Map(), dateRange)
            .get()
            .map { response =>
                    val alerts = getJsValue(response.body, "alerts")
                    val elems = for (i <- 0 until (alerts \\ "type").length)
                              yield alerts(i).validate[Alert](alertReads).get

                    //TODO: implement view
                    Ok("contents: " + elems)
            }
    }
  }

  def getIncidents() = Action {
    Async {
        getRequest("incidents", Map(), dateRange)
            .get()
            .map { response =>
                    val items = getJsValue(response.body, "incidents")
                    val elems = for (i <- 0 until (items \\ "incident_number").length)
                              yield items(i).validate[Incident](incidentReads).get

                    //TODO: implement view
                    Ok("contents: " + elems)
            }
    }
  }

  def getJsValue(contents: String, entity: String): JsValue = {
      Json.parse(contents) \ entity
  }

  def getRequest(request: String, headers: Map[String, String], params: Map[String, String]): WSRequestHolder = {
    
    val h = headers + ("Authorization" -> "Token token=%s".format(apiKey))

    var req = WS.url(endPoint + "/api/v1/" + request)
    h.map       { header => req = req.withHeaders(header) }
    params.map  { param => req = req.withQueryString(param) }
    
    Logger.debug(req.toString())
    
    req

  }

}
  
