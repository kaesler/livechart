package livechart

import com.raquo.laminar.api.L.{*, given}

// Note:
//  - singleton/module
//  - contains a Model instance
object TableAppElement:

  private val theModel = new Model

  def appElement(): Element =
    div(
      h1("Live Chart"),
      renderItemTable(),
      // Note: an extra view of the data
      renderItemList()
    )
  end appElement

  private def renderItemTable(): Element =
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
        children <-- theModel.itemListSignal.split(_.id) { (id, _, itemSignal) =>
          renderDataItem(id, itemSignal)
        }
      ),
      tfoot(
        tr(
          td(
            button(
              "➕",
              onClick --> (_ => theModel.addItem(Item()))
            )
          ),
          td(),
          td(),
          td(
            child.text <-- theModel.itemListSignal.map (
              itemList =>
                "%.2f".format(itemList.map(_.fullPrice).sum)
            )
          )
        )
      )
    )
  end renderItemTable

  private def renderItemList(): Element =
    ul(
      children <-- theModel.itemListSignal.split(_.id) {
        (_, _, itemSignal) =>
          li(
            child.text <--
              itemSignal.map(item => s"${item.count} ${item.label}")
          )
      }
    )
  end renderItemList

  private def renderDataItem(id: ItemID, itemSignal: Signal[Item]): Element =
    tr(
      td(
        inputForString(
          itemSignal.map(_.label),
          theModel.makeObserverWhichUpdatesOneItem(id) {
            (item, newLabel) =>
              if item.id == id then item.copy(label = newLabel) else item
          }
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
          onClick --> (_ => theModel.removeItem(id))
        )
      )
    )
  end renderDataItem

  // Note: this takes data model values as arguments,
  // and returns a Laminar element manipulating those values.
  // This is what many UI frameworks call a component.
  // In Laminar, components are nothing but methods manipulating
  // time-varying data and returning Laminar elements.
  private def inputForString(
    // Input values arrive here.
    valueSignal: Signal[String],
    // Output values goe here.
    valueUpdater: Observer[String]
  ): Input =
    input(
      typ := "text",
      value <-- valueSignal,
      onInput.mapToValue --> valueUpdater
    )

  end inputForString

  private def inputForDouble(
    valueSignal: Signal[Double],
    valueUpdater: Observer[Double]
  ): Input =

    // Note: this is where inputs propagate to and from where
    // new values are retrieved.
    // There is one of these contained (by reference)
    // within each INPUT element created.
    //
    val strValue = Var[String]("")

    input(
      typ := "text",

      // Note: This binder obviously belongs here because
      // it updates a DOM element when the model changes.
      value <-- strValue.signal,

      // Note: This binder obviously belongs here, because it
      // needs to propagate new values to the model when the
      // "onInput" event occurs.
      onInput.mapToValue --> strValue,

      // Note: Why is the here?
      // We put this binder here so it is lifetime-scoped by the
      // <input> element
      valueSignal --> strValue.updater[Double] {
        (prevStr, newValue) =>
          if prevStr.toDoubleOption.contains(newValue) then prevStr
          else newValue.toString
      },

      // Note: Why is the here?
      // We put this binder here so it is lifetime-scoped by the
      // <input> element
      strValue.signal --> { valueStr =>
        valueStr.toDoubleOption.foreach(valueUpdater.onNext)
      }
    )
  end inputForDouble

  private def inputForInt(
    valueSignal: Signal[Int],
    valueUpdater: Observer[Int]
  ): Input =
    input(
      typ := "text",
      controlled(
        value <-- valueSignal.map(_.toString),
        onInput.mapToValue.map(_.toIntOption).collect { case Some(newCount) =>
          newCount
        } --> valueUpdater
      )
    )
  end inputForInt

end TableAppElement
