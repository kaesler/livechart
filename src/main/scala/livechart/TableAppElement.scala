package livechart

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.annotation.*

// Note:
//  - singleton/module
//  - contains a Model instance
object TableAppElement:

  private val theModel = new Model

  def appElement(): Element =
    div(
      h1("Live Chart"),
      renderItemTable(),
      renderItemChart(),
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
          renderItem(id, itemSignal)
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
            child.text <-- theModel.itemListSignal.map { itemList =>
              "%.2f".format(itemList.map(_.fullPrice).sum)
            }
          )
        )
      )
    )
  end renderItemTable

  private def renderItemList(): Element =
    ul(
      children <-- theModel.itemListSignal.split(_.id) { (_, _, itemSignal) =>
        li(
          child.text <--
            itemSignal.map(item => s"${item.count} ${item.label}")
        )
      }
    )
  end renderItemList

  private def renderItem(id: ItemID, itemSignal: Signal[Item]): Element =
    tr(
      td(
        inputForString(
          itemSignal.map(_.label),
          theModel.makeObserverToUpdateAnItemLabel(id)
        )
      ),
      td(
        inputForDouble(
          itemSignal.map(_.price),
          theModel.makeObserverToUpdateAnItemPrice(id)
        )
      ),
      td(
        inputForInt(
          itemSignal.map(_.count),
          theModel.makeObserverToUpdateAnItemCount(id)
        )
      ),
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
  end renderItem

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
      valueSignal --> strValue.updater[Double] { (prevStr, newValue) =>
        if prevStr.toDoubleOption.contains(newValue) then prevStr
        else newValue.toString
      },

      // Note: Why is the here?
      // We put this binder here so it is lifetime-scoped by the
      // <input> element
      strValue.signal --> { valueStr =>
        // Note: we only propagate to the model inputs corresponding
        // to valid Double values.
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
      // Note: a controlled element is a way to ensure that the value property
      // is locked to a certain Observable.
      // In this case we use ".toIntOption" and ".collect" so that only inputs
      // that can be parsed as valid Int values are accepted and propagated
      // to the model.
      controlled(
        value <-- valueSignal.map(_.toString),
        onInput.mapToValue.map(_.toIntOption).collect { case Some(newCount) =>
          newCount
        } --> valueUpdater
      )
    )
  end inputForInt

  private val chartConfig =
    import typings.chartJs.mod.*
    new ChartConfiguration {
      `type` = ChartType.bar
      data = new ChartData {
        datasets = js.Array(
          new ChartDataSets {
            label = "Price"
            borderWidth = 1
            backgroundColor = "green"
          },
          new ChartDataSets {
            label = "Full price"
            borderWidth = 1
            backgroundColor = "blue"
          }
        )
      }
      options = new ChartOptions {
        scales = new ChartScales {
          yAxes = js.Array(
            new CommonAxe {
              ticks = new TickOptions {
                beginAtZero = true
              }
            }
          )
        }
      }
    }
  end chartConfig

  private def renderItemChart(): Element =
    import scala.scalajs.js.JSConverters.*
    import typings.chartJs.mod.*

    var optChart: Option[Chart] = None

    canvasTag(
      // Regular properties of the canvas
      width  := "100%",
      height := "200px",

      // onMountUnmount callback to bridge the Laminar world and the Chart.js world
      onMountUnmountCallback(
        // on mount, create the `Chart` instance and store it in optChart
        mount = nodeCtx =>
          val domCanvas: dom.HTMLCanvasElement = nodeCtx.thisNode.ref
          val chart = Chart.apply.newInstance2(domCanvas, chartConfig)
          optChart = Some(chart)
        ,
        // on unmount, destroy the `Chart` instance
        unmount = _ =>
          optChart.foreach(_.destroy())
          optChart = None
      ),

      // Bridge the FRP world of dataSignal to the imperative world of the
      // `chart.data`.
      theModel.itemListSignal --> { items =>
        for (chart <- optChart) {
          chart.data.labels = items.map(_.label).toJSArray
          chart.data.datasets.get(0).data = items.map(_.price).toJSArray
          chart.data.datasets.get(1).data = items.map(_.fullPrice).toJSArray
          chart.update()
        }
      }
    )
  end renderItemChart

end TableAppElement
