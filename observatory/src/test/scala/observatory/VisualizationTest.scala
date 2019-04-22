package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait VisualizationTest extends FunSuite with Checkers {

  test("test greate circle distance : antipod") {
    val l1 = Location(0, 0)
    val l2 = Location(0, 180)
    val distance = Visualization.greatCircleDistance(l1, l2)
    val expectedDistance = 20015
    assert(expectedDistance == math.round(distance), "Can't instantiate a StackOverflow object")
  }

  test("test greate circle distance") {
    val l1 = Location(48.794, -122.537)
    val l2 = Location(34.708, -77.44)
    val distance = Visualization.greatCircleDistance(l1, l2)
    val expectedDistance = 3985
    assert(expectedDistance == math.round(distance), "Can't instantiate a StackOverflow object")
  }
  
  test("color linear Interpolation") {
    val data = List((-60.0, Color(0,0,0)), 
                    (-50.0, Color(33,0,107)), 
                    (-27.0, Color(255,0,255)),
                    (-15.0, Color(0,0,255)),
                    (0.0, Color(0,255,255))).toIterable
    val temp = -20
    val expectedColor = Color(106, 0, 255)
    assert(Visualization.interpolateColor(data, temp) == expectedColor, "color did not match")
  }

}
