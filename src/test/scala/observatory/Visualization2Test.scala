package observatory

import org.junit.Test

trait Visualization2Test extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("value-added information visualization", 5) _

  @Test def `test visualizeGrid produces an image of 256×256 pixels`: Unit = {
    val image = Visualization2.visualizeGrid(_ => 10, List(), Tile(0,0,0))
    assert(image.height == 256)
    assert(image.width == 256)
  }


}
