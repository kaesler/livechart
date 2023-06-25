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
        // Note: use split() to only render new elements, rather than
        // re-rendering he whole list.
        children <-- dataSignal.split(_.id) { (id, _, itemSignal) =>
          renderDataItem(id, itemSignal)
        }
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

  private def renderDataItem(id: DataItemID, itemSignal: Signal[DataItem]): Element =
    tr(
      td(child.text <-- itemSignal.map(_.label)),
      td(child.text <-- itemSignal.map(_.price)),
      td(child.text <-- itemSignal.map(_.count)),
      td(
        child.text <-- itemSignal.map(item => "%.2f".format(item.fullPrice))
      ),
      td(
        button(
          "🗑️ trashit",
          onClick --> (_ => removeDataItem(id))
        )
      )
    )
  end renderDataItem

end TableAppElement
