package livechart

import com.raquo.laminar.api.L.{*, given}

final class Model:
  private val itemListVar: Var[ItemList] = Var(
    List(
      Item(ItemID(), "one", 1.0, 1)
    )
  )

  // Accessor: stream of signals as the DataList changes.
  val itemListSignal: StrictSignal[ItemList] = itemListVar.signal

  // Makes a "mutating" (in the functional sense) observer
  // of a stream of A values, for a DataItem specified by Id.
  def makeObserverWhichUpdatesOneItem[A](id: ItemID)(
    f: (Item, A) => Item
  ): Observer[A] =
    itemListVar.updater[A]: (oldItemList, newA) =>
      // Note: compute a new DataList.
      oldItemList.map: item =>
        if item.id == id then f(item, newA) else item
  end makeObserverWhichUpdatesOneItem

  // Mutator.
  def addItem(item: Item): Unit =
    itemListVar.update(itemList => itemList :+ item)

  // Mutator.
  def removeItem(id: ItemID): Unit =
    itemListVar.update(itemList => itemList.filter(_.id != id))
end Model
