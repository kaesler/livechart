package livechart

import scala.util.Random

final class ItemID

case class Item(id: ItemID, label: String, price: Double, count: Int):
  def fullPrice: Double = price * count

object Item:
  def apply(): Item =
    Item(ItemID(), "?", Random.nextDouble(), Random.nextInt(5) + 1)
end Item

type ItemList = List[Item]
