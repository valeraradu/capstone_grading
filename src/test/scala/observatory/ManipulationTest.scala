package observatory

import org.junit.Test

trait ManipulationTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("data manipulation", 4) _

  // Implement tests for methods of the `Manipulation` object



 /*
   [Test Description] average must return a grid whose predicted temperatures are the average of the known temperatures (4pts)(observatory.CapstoneSuite)
    [Observed Error] Invalid predicted temperature at (90, -180): 23.599904068111883. Expected: 9.499760170279702.
*/

  @Test def `average must return a grid whose predicted temperatures are the average of the known temperatures `: Unit = {
    val average = Manipulation.average(List(List((Location(0,0), 10.0), (Location(0,0), 0.0))))(GridLocation(0, 0))
    assert(average == 10.0, s"expected 10 ut was $average")
  }

  @Test def `average must return a grid whose predicted temperatures are the average of the known temperatures over years`: Unit = {
    val average = Manipulation.average(List(List((Location(0,0), 10.0)), List((Location(0,0), 0.0))))(GridLocation(0, 0))
    assert(average == 5.0, s"expected 5 ut was $average")
  }

}
