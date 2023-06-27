package livechart

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.annotation.*

// import javascriptLogo from "/javascript.svg"
@js.native @JSImport("/javascript.svg", JSImport.Default)
val javascriptLogo: String = js.native

@main
def pageMain(): Unit =
  renderOnDomContentLoaded(
    dom.document.getElementById("app"),
    // ButtonAppElement.appElement()
    TableAppElement.appElement()
  )
