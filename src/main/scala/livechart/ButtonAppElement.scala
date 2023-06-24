package livechart

import com.raquo.laminar.api.L.{*, given}

object ButtonAppElement:
  def appElement(): Element =
    div(
      a(
        href   := "https://vitejs.dev",
        target := "_blank",
        img(src := "/vite.svg", className := "logo", alt := "Vite logo")
      ),
      a(
        href   := "https://developer.mozilla.org/en-US/docs/Web/JavaScript",
        target := "_blank",
        img(src := javascriptLogo, className := "logo vanilla", alt := "JavaScript logo")
      ),
      h1("Hello Laminar!(from Kevo)"),
      div(className := "card", counterButton()),
      p(className   := "read-the-docs", "Click on the Vite logo to learn more")
    )
  end appElement

  private def counterButton(): Element =
    val counter = Var(0)
    button(
      tpe := "button",
      "count is ",
      child.text <-- counter,
      onClick --> { _ => counter.update(c => c + 1) }
    )
  end counterButton

end ButtonAppElement
