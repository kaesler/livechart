package livechart

import com.raquo.laminar.api.L.{*, given}

// Note: this is mutable.
//  - contains Var
//  - has an algebra for mutating
//  - exports a StrictSignal
final class Model:
  private val dataVar: Var[DataList]     = Var(List(DataItem(DataItemID(), "one", 1.0, 1)))
  val dataSignal: StrictSignal[DataList] = dataVar.signal

  def addDataItem(item: DataItem): Unit =
    dataVar.update(data => data :+ item)

  def removeDataItem(id: DataItemID): Unit =
    dataVar.update(data => data.filter(_.id != id))
end Model
