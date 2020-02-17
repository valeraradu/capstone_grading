package observatory

import java.time.LocalDate

import org.junit.Assert._
import org.junit.{Rule, Test}

trait ExtractionTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("data extraction", 1) _

  // Implement tests for the methods of the `Extraction` object
  // For implicit conversions like converting RDDs to DataFrames
  @Test def `test locateTemperatures`: Unit = {
    assert(
      Extraction.locateTemperatures(2015, "/stations.csv", "/2015_test.csv") ==
        Seq(
          (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.3),
          (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0),
          (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.0)
        ))

  }

  @Test def `test locationYearlyAverageRecords`: Unit = {
    assert(
      Extraction.locationYearlyAverageRecords(
          Seq(
            (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.3),
            (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0),
            (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.0)
          )).toSet ==
        Seq(
          (Location(37.35, -78.433), 27.3),
          (Location(37.358, -78.438), 1.0)
        ).toSet)

  }


  @Rule
  def individualTestTimeout = new org.junit.rules.Timeout(100 * 1000000)


}
