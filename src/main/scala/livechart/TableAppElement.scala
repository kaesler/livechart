package livechart

import com.raquo.laminar.api.L.{*, given}

// Note:
//  - singleton/module
//  - contains a Model instance
object TableAppElement:

  private val model = new Model
  import model.*

  def appElement(): Element =
    div(
      h1("Live Chart"),
      renderDataTable()
    )
  end appElement

  private def renderDataTable(): Element =
    table(
      thead(
        tr(
          th("Label"),
          th("Price"),
          th("Count"),
          th("Full price"),
          th("Action")
        )
      ),
      tbody(
        children <-- dataSignal.map:
          _.map:
            item => renderDataItem(item.id, item)
      ),
      tfoot(
        tr(
          td(
            button(
              "➕",
              onClick --> (_ => addDataItem(DataItem()))
            )
          ),
          td(),
          td(),
          td(
            child.text <-- dataSignal.map: data =>
              "%.2f".format(data.map(_.fullPrice).sum)
          )
        )
      )
    )
  end renderDataTable

  private def renderDataItem(id: DataItemID, item: DataItem): Element =
    tr(
      td(item.label),
      td(item.price),
      td(item.count),
      td("%.2f".format(item.fullPrice)),
      td(
        button(
          "🗑️ trash",
          onClick --> (_ => removeDataItem(id))
        )
      )
    )
  end renderDataItem

end TableAppElement
