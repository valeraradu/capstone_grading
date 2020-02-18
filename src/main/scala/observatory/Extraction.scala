package observatory

import java.time.LocalDate

import org.apache.log4j.{Level, Logger}
import org.apache.spark.sql.DataFrame
import org.apache.spark.sql.types.{DoubleType, IntegerType, StructField, StructType}

import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface {

  @transient
  lazy val spark = org.apache.spark.sql.SparkSession.builder
    .master("local")
    .appName("Spark CSV Reader.")
    .getOrCreate

  import spark.implicits._

  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)


  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year,
                         stationsFile: String,
                         temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {

    def readDS(path: String, schema: StructType): DataFrame = {
      spark.read
        .option("header", "false")
        .schema(schema)
        .csv(Source.fromInputStream(this.getClass.getResourceAsStream(path), "utf-8").getLines().toList.toDS())
    }

    val stations = readDS(stationsFile, StructType(List(
      StructField("STN", IntegerType),
      StructField("WBAN", IntegerType),
      StructField("lat", DoubleType),
      StructField("lon", DoubleType))))

    val temperatures = readDS(temperaturesFile, StructType(List(
      StructField("STN", IntegerType),
      StructField("WBAN", IntegerType),
      StructField("month", IntegerType),
      StructField("day", IntegerType),
      StructField("temp", DoubleType)))
    )

    stations.filter(stations("lat").isNotNull && stations("lon").isNotNull)
      .join(temperatures, stations("STN") <=> temperatures("STN") && stations("WBAN") <=> temperatures("WBAN"))
      .collect.map(
        value => (LocalDate.of(year, value.getInt(6), value.getInt(7)),
        Location(value.getDouble(2), value.getDouble(3)), Math.round(((value.getDouble(8) - 32.0) * 5.0/9 ) * 100) / 100.0 )
      )

  }


  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {

    records.groupBy(record => record._2).map(r => (r._1, r._2.map(_._3).sum/r._2.size))
  }

}
