package observatory

import org.junit.Assert._
import org.junit.{Rule, Test}

trait InteractionTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("interactive visualization", 3) _

  @Test def `test tile produces an image of 256×256 pixels`: Unit = {

    val image = Interaction.tile(List(), List(), Tile(0,0,0))

    assert(image.height == 256)
    assert(image.width == 256)

  }

}
