object Exercises {

    /* ПРИМЕР */
    def divBy3Or7(iFrom: Int, iTo: Int): Seq[Int] = {
        for {
            i <- iFrom to iTo
            if i % 3 == 0 || i % 7 == 0
        } yield i
    }

    /* ЗАДАНИЕ I */
    def sumOfDivBy3Or5(iFrom: Int, iTo: Int): Long = {
        (iFrom to iTo)
          .filter(i => i % 3 == 0 || i % 5 == 0)
          .map(_.toLong)
          .sum
    }

    /* ЗАДАНИЕ II */
    def primeFactor(number: Int): Seq[Int] = {
        def isPrime(n: Int): Boolean =
            if (n < 2) false else !(2 until n).exists(n % _ == 0)

        def factors(n: Int, divisor: Int = 2, acc: Set[Int] = Set.empty): Set[Int] = {
            if (n < 2) acc
            else if (n % divisor == 0)
                factors(n / divisor, divisor, acc + divisor)
            else if (divisor * divisor > n)
                if (n > 1) acc + n else acc
            else
                factors(n, divisor + 1, acc)
        }

        factors(number).toSeq.sorted
    }

    /* ЗАДАНИЕ III */
    case class Vector2D(x: Double, y: Double)

    def abs(vec: Vector2D): Double = math.sqrt(vec.x * vec.x + vec.y * vec.y)
    def scalar(vec0: Vector2D, vec1: Vector2D): Double = vec0.x * vec1.x + vec0.y * vec1.y
    def cosBetween(vec0: Vector2D, vec1: Vector2D): Double =
        scalar(vec0, vec1) / abs(vec0) / abs(vec1)

    // Универсальная функция
    def sumByFunc(
                   leftVec0: Vector2D, leftVec1: Vector2D,
                   f: (Vector2D, Vector2D) => Double,
                   rightVec0: Vector2D, rightVec1: Vector2D
                 ): Double = {
        f(leftVec0, leftVec1) + f(rightVec0, rightVec1)
    }

    def sumScalars(leftVec0: Vector2D, leftVec1: Vector2D,
                   rightVec0: Vector2D, rightVec1: Vector2D): Double =
        sumByFunc(leftVec0, leftVec1, scalar, rightVec0, rightVec1)

    def sumCosines(leftVec0: Vector2D, leftVec1: Vector2D,
                   rightVec0: Vector2D, rightVec1: Vector2D): Double =
        sumByFunc(leftVec0, leftVec1, cosBetween, rightVec0, rightVec1)

    /* ЗАДАНИЕ IV */
    val balls: Map[String, (Int, Double)] =
        Map(
            "Aluminum" -> (3,   2.6889), "Tungsten" ->  (2,   19.35), "Graphite" ->  (12,  2.1),   "Iron" ->      (3,   7.874),
            "Gold" ->     (2,   19.32),  "Potassium" -> (14,  0.862), "Calcium" ->   (8,   1.55),  "Cobalt" ->    (4,   8.90),
            "Lithium" ->  (12,  0.534),  "Magnesium" -> (10,  1.738), "Copper" ->    (3,   8.96),  "Sodium" ->    (5,   0.971),
            "Nickel" ->   (2,   8.91),   "Tin" ->       (1,   7.29),  "Platinum" ->  (1,   21.45), "Plutonium" -> (3,   19.25),
            "Lead" ->     (2,   11.336), "Titanium" ->  (2,   10.50), "Silver" ->    (4,   4.505), "Uranium" ->   (2,   19.04),
            "Chrome" ->   (3,   7.18),   "Cesium" ->    (7,   1.873), "Zirconium" -> (3,   6.45)
        )

    def sortByHeavyweight(ballsArray: Map[String, (Int, Double)] = balls): Seq[String] = {
        def volume(r: Int): Double = (4.0 / 3.0) * math.Pi * math.pow(r, 3)
        ballsArray
          .toSeq
          .sortBy { case (_, (r, d)) => volume(r) * d }
          .map(_._1)
    }
}
