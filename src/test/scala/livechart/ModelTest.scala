package livechart

class ModelTest extends munit.FunSuite:
  test("fullPrice") {
    val item = Item(ItemID(), "test", 0.5, 5)
    assert(item.fullPrice == 2.5)
  }

  test("addItem") {
    val model = new Model

    val item = Item(ItemID(), "test", 0.5, 2)
    model.addItem(item)

    val afterItems = model.itemListSignal.now()
    assert(afterItems.size == 2)
    assert(afterItems.last == item)
  }

  test("removeItem") {
    val model = new Model

    model.addItem(Item(ItemID(), "test", 0.5, 2))

    val beforeItems = model.itemListSignal.now()
    assert(beforeItems.size == 2)

    model.removeItem(beforeItems.head.id)

    val afterItems = model.itemListSignal.now()
    assert(afterItems.size == 1)
    assert(afterItems == beforeItems.tail)
  }
end ModelTest

