package livechart

import com.raquo.laminar.api.L.{*, given}

final class Model:
  private val dataVar: Var[DataList] = Var(List(DataItem(DataItemID(), "one", 1.0, 1)))

  // Accessor: stream of signals as the DataList changes.
  val dataSignal: StrictSignal[DataList] = dataVar.signal

  // Makes a "mutating" (in the functional sense) observer
  // for a DataItem specified by Id.
  def makeObserverWhichUpdatesItemWithGivenId[A](id: DataItemID)(
    f: (DataItem, A) => DataItem
  ): Observer[A] =
    dataVar.updater[A]: (dataList, a) =>
      dataList.map: item =>
        if item.id == id then f(item, a) else item
  end makeObserverWhichUpdatesItemWithGivenId

  // Mutator.
  def addDataItem(item: DataItem): Unit =
    dataVar.update(data => data :+ item)

  // Mutator.
  def removeDataItem(id: DataItemID): Unit =
    dataVar.update(data => data.filter(_.id != id))
end Model
