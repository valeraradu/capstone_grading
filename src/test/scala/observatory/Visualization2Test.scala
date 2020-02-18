package observatory

import org.junit.Test

trait Visualization2Test extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("value-added information visualization", 5) _

  @Test def `test visualizeGrid produces an image of 256×256 pixels`: Unit = {
    val image = Visualization2.visualizeGrid(_ => 10, List(), Tile(0,0,0))
    assert(image.height == 256)
    assert(image.width == 256)
  }

  @Test def `interpolated Expected 0E-23 (d00 = 0.0, d01 = 0.0, d10 = 0.0, d11 = -1.0021297162650767E-5, x = 0.0, y = 0.0)`: Unit = {
    val biinterpolated = Visualization2.bilinearInterpolation(CellPoint(0.0, 0.0), 0.0, 0.0, 0.0 , 0.0)
    assert(biinterpolated == 0E-23, s"biinterpolated expected 0E-23 but was ${biinterpolated}")
  }

  @Test def `interpolated Expected 1.52587890625E-7 (d00 = 0.0, d01 = 0.0, d10 = 0.0, d11 = 1.52587890625E-5, x = 0.1, y = 0.1)`: Unit = {
    val biinterpolated = Visualization2.bilinearInterpolation(CellPoint(0.1, 0.1), 0.0, 0.0, 0.0 , 1.52587890625E-5)
    assert(biinterpolated == 1.52587890625E-7, s"biinterpolated expected 1.52587890625E-7 but was ${biinterpolated}")
  }

}
