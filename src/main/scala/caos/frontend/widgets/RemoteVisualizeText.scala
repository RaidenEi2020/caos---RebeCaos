package caos.frontend
package widgets

import caos.view.View
import org.scalajs.dom.{document, html}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

class RemoteVisualizeText[Stx](buildCmd: Either[(Stx, Stx => String) => String, String => String], parseToString: Option[Stx => String], raw: () => String, parsedRaw: () => Stx, generateHtml: String => String, remember: Boolean, service: String,
                               name: String, errorBox: OutputArea, doc: Documentation)
  extends Widget[Unit](name, doc):

  override val get: Unit = {}
  protected val divBox = name.replace(' ', '_') + "Box"
  private val token = generateToken(32)
  var firstTime = true
  private var box: Block = _
  private var txt: Block = _
  private var url: String = ""

  /**
   * Executed once at creation time, to append the content to the inside of this box
   *
   * @param div     Placeholder that will receive the "append" with the content of the box
   * @param visible is true when this box is initially visible (i.e., expanded).
   */
  override def init(div: Block, visible: Boolean): Unit = {
    val down: (Either[String, String], (() => Unit, String)) =
      Left("clear") -> (
        () => clear,
        "Clear")

    box = panelBox(div, visible, buttons = down :: Nil).append("div")
      .attr("id", divBox)
  }

  /**
   * Block of code that should read the dependencies and:
   *  - update its output value, and
   *  - produce side-effects (e.g., redraw a diagram)
   */
  override def update(): Unit =
    if (isVisible || !firstTime) then
      try {
        if (isVisible) {
          firstTime = false
          url = buildUrl
          fetchFromServer(url).onComplete {
            case Success(reply) => createAndAppendDiv(generateHtml(reply))
            case Failure(e) => e
          }
        }
      }
      catch Widget.checkExceptions(errorBox, name)

  def generateToken(length: Int): String = {
    val chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    (1 to length).map { _ =>
      chars((Math.random() * chars.length).toInt)
    }.mkString
  }

  def clear: Unit = {
    val widgetDiv = document.getElementById(divBox).asInstanceOf[html.Div]
    val children = widgetDiv.getElementsByTagName("div")
    while (children.length > 0) {
      widgetDiv.removeChild(children(0))
    }
  }

  def buildUrl: String = {
    val cmd = buildCmd match {
      case Left(f) => f(parsedRaw(), parseToString.get)
      case Right(f) => f(raw())
    }
    s"http://$service/run-process?cmd=${java.net.URLEncoder.encode(cmd, "UTF-8")}&token=$token"
  }

  def fetchFromServer(url: String): Future[String] = {
    org.scalajs.dom.window.fetch(url)
      .toFuture
      .map { response =>
        if (response.ok)
          response.text().toFuture
        else
          sys.error("Failed to fetch data from the server")
      }
      .flatten
  }

  def createAndAppendDiv(htmlDiv: String): Unit = {
    if (!remember) {
      clear
    }
    val replyDiv = document.createElement("div").asInstanceOf[html.Div]
    replyDiv.innerHTML = htmlDiv
    replyDiv.setAttribute("style", "border: 3px solid #ccc; padding: 0.5em; margin-bottom: 0.5em; border-radius: 6px;")
    val widgetDiv = document.getElementById(divBox).asInstanceOf[html.Div]
    widgetDiv.appendChild(replyDiv)
  }
