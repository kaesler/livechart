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
      renderDataTable(),
      // Note: an extra view of the data
      renderDataList()
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

  private def renderDataList(): Element =
    ul(
      children <-- dataSignal.split(_.id): (_, _, itemSignal) =>
        li(child.text <-- itemSignal.map(item => s"${item.count} ${item.label}"))
    )
  end renderDataList

  private def renderDataItem(id: DataItemID, itemSignal: Signal[DataItem]): Element =
    tr(
      td(
        inputForString(
          itemSignal.map(_.label),
          makeItemUpdater[String](
            id,
            (item, newLabel) => if item.id == id then item.copy(label = newLabel) else item
          )
        )
      ),
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

  private def makeItemUpdater[A](
    id: DataItemID,
    f: (DataItem, A) => DataItem
  ): Observer[A] =
    dataVar.updater[A]: (dataList, a) =>
      dataList.map: item =>
        if item.id == id then f(item, a) else item
  end makeItemUpdater

  // Note: this takes data model values as arguments,
  // and returns a Laminar element manipulating those values.
  // This is what many UI frameworks call a component.
  // In Laminar, components are nothing but methods manipulating
  // time-varying data and returning Laminar elements.
  private def inputForString(
    valueSignal: Signal[String],
    valueUpdater: Observer[String]
  ): Input =
    input(
      typ := "text",
      value <-- valueSignal,
      onInput.mapToValue --> valueUpdater
    )
  end inputForString

end TableAppElement
