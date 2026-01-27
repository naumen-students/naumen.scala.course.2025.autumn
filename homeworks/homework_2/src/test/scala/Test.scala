import utest._
import Exercises._

object Test extends TestSuite{
    val tests = Tests{
        'test_divBy3Or7 - {
            assert(divBy3Or7(1, 3) == Seq(3))
            assert(divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }

        'test_sumOfDivBy3Or5 - {
            assert(sumOfDivBy3Or5(1, 3) == 3)
            assert(sumOfDivBy3Or5(1, 5) == 8)
            assert(sumOfDivBy3Or5(1, 6) == 14)
            assert(sumOfDivBy3Or5(14, 16) == 15)
            assert(sumOfDivBy3Or5(29, 31) == 30)
            assert(sumOfDivBy3Or5(-5, 5) == 0)
            assert(sumOfDivBy3Or5(3, 3) == 3)
            assert(sumOfDivBy3Or5(4, 4) == 0)
            assert(sumOfDivBy3Or5(5, 3) == 0)
            assert(sumOfDivBy3Or5(1, 9) == 23)
            assert(sumOfDivBy3Or5(0, 100) == 2418)
        }
        
        'test_primeFactor - {
            assert(primeFactor(1) == Seq())
            assert(primeFactor(0) == Seq())
            assert(primeFactor(-5) == Seq())
            assert(primeFactor(2) == Seq(2))
            assert(primeFactor(3) == Seq(3))
            assert(primeFactor(13) == Seq(13))
            assert(primeFactor(17) == Seq(17))
            assert(primeFactor(80) == Seq(2, 5))
            assert(primeFactor(98) == Seq(2, 7))
            assert(primeFactor(12) == Seq(2, 3))
            assert(primeFactor(18) == Seq(2, 3))
            assert(primeFactor(30) == Seq(2, 3, 5))
            assert(primeFactor(36) == Seq(2, 3))
            assert(primeFactor(100) == Seq(2, 5))
            assert(primeFactor(8) == Seq(2))
            assert(primeFactor(27) == Seq(3))
            assert(primeFactor(16) == Seq(2))
            assert(primeFactor(81) == Seq(3))
            assert(primeFactor(2310) == Seq(2, 3, 5, 7, 11))
            assert(primeFactor(1001) == Seq(7, 11, 13))
        }

        'test_sumScalars - {
            assert(sumScalars(Vector2D(0, 0), Vector2D(0, 0), Vector2D(0, 0), Vector2D(0, 0)) == 0.0)
            assert(sumScalars(Vector2D(1, 0), Vector2D(1, 0), Vector2D(0, 1), Vector2D(0, 1)) == 2.0)
            assert(sumScalars(Vector2D(1, 2), Vector2D(3, 4), Vector2D(5, 6), Vector2D(7, 8)) ==
              scalar(Vector2D(1, 2), Vector2D(3, 4)) + scalar(Vector2D(5, 6), Vector2D(7, 8)))
            assert(sumScalars(Vector2D(-1, -2), Vector2D(-3, -4), Vector2D(1, 2), Vector2D(3, 4)) == 22.0)
        }

        'test_sumCosines - {
            assert(sumCosines(Vector2D(1, 0), Vector2D(2, 0), Vector2D(0, 1), Vector2D(0, 3)) == 2.0)
            assert(sumCosines(Vector2D(1, 0), Vector2D(0, 1), Vector2D(0, 1), Vector2D(1, 0)) == 0.0)

            val cos1 = cosBetween(Vector2D(1, 0), Vector2D(-1, 0))
            val cos2 = cosBetween(Vector2D(0, 1), Vector2D(0, -1))
            assert(sumCosines(Vector2D(1, 0), Vector2D(-1, 0), Vector2D(0, 1), Vector2D(0, -1)) == cos1 + cos2)

            assert(sumCosines(Vector2D(1, 1), Vector2D(1, 1), Vector2D(2, 3), Vector2D(2, 3)) == 2.0)
        }

        'test_sortByHeavyweight - {
            assert(sortByHeavyWeight(Map.empty) == Seq.empty)
            
            val singleBall = Map("Water" -> (1, 1.0))
            assert(sortByHeavyWeight(singleBall) == Seq("Water"))
            
            val BallsWithSameRadius = Map(
                "Light" -> (2, 1.0),
                "Heavy" -> (2, 10.0)
            )
            assert(sortByHeavyWeight(BallsWithSameRadius) == Seq("Light", "Heavy"))
            
            val ballsWithSameDensity = Map(
                "Big" -> (3, 5.0),
                "Small" -> (1, 5.0)
            )
            assert(sortByHeavyWeight(ballsWithSameDensity) == Seq("Small", "Big"))
            
            val twoBalls = Map(
                "SmallHeavy" -> (2, 100.0),
                "BigLight" -> (10, 1.0)
            )
            assert(sortByHeavyWeight(twoBalls) == Seq("SmallHeavy", "BigLight"))
            
            assert(sortByHeavyWeight(balls) == Seq(
                "Tin",
                "Platinum",
                "Nickel",
                "Aluminum",
                "Titanium",
                "Lead",
                "Sodium",
                "Uranium",
                "Gold",
                "Tungsten",
                "Zirconium",
                "Chrome",
                "Iron",
                "Copper",
                "Silver",
                "Plutonium",
                "Cobalt",
                "Cesium",
                "Calcium",
                "Lithium",
                "Magnesium",
                "Potassium",
                "Graphite"
            ))
        }
    }
}
