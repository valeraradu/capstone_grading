package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.util.DistanceCalculatorImpl
import org.apache.commons.math3.ml.distance.DistanceMeasure

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  /*
    def CalcIDWvalue(unknowncell, knowncells):
      weighted_values_sum = 0.0
      sum_of_weights = 0.0
      neg_half_sens = -sensitivity/2.0
      for knowncell in knowncells:
        weight = ((unknowncell.x-knowncell.x)**2 + (unknowncell.y-knowncell.y)**2)**neg_half_sens
        sum_of_weights += weight
        weighted_values_sum += weight * knowncell.value
      return weighted_values_sum / sum_of_weights
  */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val p = 2
    val temps =
      temperatures.map(temp => (DistanceCalculatorImpl.calculateDistanceInKilometer(location, temp._1), temp._2))

    temps.find(temp => temp._1 <= 1).getOrElse({
      val (sum, weights) = temps.foldLeft((0.0, 0.0))((agg, temp) => {
        val weight = Math.pow(temp._1, -p)
        (agg._1 + weight, agg._2 + weight * temp._2)
      }
    )
      (0.0, weights/sum)
    })._2
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {

    val(lower, upper) = points.foldLeft(((0.0, Color(-1, -1, -1)), (9999.0, Color(-1, -1, -1))))((minmaxcolors, point) => {
      (if( point._1 <= value && point._1 >  minmaxcolors._1._1 ) point else minmaxcolors._1,
      if( point._1 >= value && point._1 <  minmaxcolors._2._1 ) point else minmaxcolors._2)
    })

    if(lower == upper){
      lower._2
    } else {
      val weightLower = (value - lower._1)/(upper._1 - lower._1)
      val weightUper = (upper._1 - value)/(upper._1 - lower._1)

      Color((lower._2.red * weightLower + upper._2.red * weightUper).toInt,
        (lower._2.green * weightLower + upper._2.green * weightUper).toInt,
        (lower._2.blue * weightLower + upper._2.blue * weightUper).toInt)

    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {

    val pixels = for {
      x <- 0 to 359
      y <- 0 to 179
    } yield {
      val color = interpolateColor(colors, predictTemperature(temperatures, coordToGeo(x, y)))
        Pixel(color.red, color.green, color.blue, 255)
    }

    Image(360, 180, pixels.toArray)

  }

  //https://stackoverflow.com/questions/14329691/convert-latitude-longitude-point-to-a-pixels-x-y-on-mercator-projection
  //https://github.com/mraad/WebMercator/blob/master/src/main/java/com/esri/WebMercator.java
  /*
      Pseudo code example, so this can be adapted to every programming language.

      latitude    = 41.145556; // (φ)
      longitude   = -73.995;   // (λ)

      mapWidth    = 200;
      mapHeight   = 100;

      // get x value
      x = (longitude+180)*(mapWidth/360)

      // convert from degrees to radians
      latRad = latitude*PI/180;

      // get y value
      mercN = ln(tan((PI/4)+(latRad/2)));
      y     = (mapHeight/2)-(mapWidth*mercN/(2*PI));
  */
  def geoToCoord(location: Location): (Int, Int) = {
    val mapWidth    = 360;
    val mapHeight   = 180;

    val x = (location.lon + 180)*(mapWidth/360)

    // convert from degrees to radians
    //val latRad = location.lat * Math.PI/180;

    // get y value
    //val mercN = Math.log(Math.tan((Math.PI/4)+(latRad/2)))
    //val y     = (mapHeight/2)-(mapHeight*mercN/(2*Math.PI));

    //https://github.com/mfeldheim/hermap/blob/master/src/Geo/Projection.php
    //'x' => ($lng+180)*($width/360),
    //'y' => ($height/2)-($width*log(tan((M_PI/4)+(($lat*M_PI/180)/2)))/(2*M_PI))
    //val y = (mapHeight/2)-(mapWidth*Math.log(Math.tan((Math.PI/4)+((location.lat*Math.PI/180)/2)))/(2*Math.PI))
    val y  = mapHeight/2 -((location.lat * mapHeight) / 180)
    (x.toInt, y.toInt)
  }

  def coordToGeo(x: Int, y: Int): Location = {
    val mapWidth    = 360;
    val mapHeight   = 180;
    val lon = x/(mapWidth/360)-180
    val lat = -((y - mapHeight/2)*180)/mapHeight
    Location(lat, lon)
  }

}

