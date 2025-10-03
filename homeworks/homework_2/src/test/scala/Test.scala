import utest._

class SumTest extends AnyFlatSpec with Matchers {
    
    "sumOfDivBy3Or5" should "work for 1 to 10" in {
        sumOfDivBy3Or5(1, 10) shouldBe 33L  
    }
    
    it should "work when from > to" in {
        sumOfDivBy3Or5(10, 1) shouldBe 33L
    }
    
    it should "work for single number" in {
        sumOfDivBy3Or5(3, 3) shouldBe 3L
    }
    
    it should "return 0 when no matches" in {
        sumOfDivBy3Or5(1, 2) shouldBe 0L
    }
}

class PrimeFactorTest extends AnyFlatSpec with Matchers {
    
    "primeFactor" should "return Seq(2, 5) for 80" in {
        primeFactor(80) shouldBe Seq(2, 5)
    }
    
    it should "return Seq(2, 7) for 98" in {
        primeFactor(98) shouldBe Seq(2, 7)
    }
    
    it should "return Seq(2, 3, 5) for 30" in {
        primeFactor(30) shouldBe Seq(2, 3, 5)  
    }
    
    it should "return Seq(2, 3) for 24" in {
        primeFactor(24) shouldBe Seq(2, 3)  
    }
    
    it should "return Seq(7) for 49" in {
        primeFactor(49) shouldBe Seq(7)  
    }

class VectorTest extends AnyFlatSpec with Matchers {
    
    val vec1 = Vector2D(1, 0)
    val vec2 = Vector2D(0, 1)
    val vec3 = Vector2D(1, 1)
    val vec4 = Vector2D(2, 2)
    
    "sumScalars" should "calculate sum of scalar products" in {
        sumScalars(vec1, vec2, vec3, vec4) shouldBe 4.0
    }
    
    it should "work with parallel vectors" in {
        sumScalars(vec1, vec1, vec3, vec3) shouldBe 3.0
    }
    
    it should "work with perpendicular vectors" in {
        sumScalars(vec1, vec2, vec2, vec1) shouldBe 0.0
    }

     "sumCosines" should "calculate sum of cosines" in {
        sumCosines(vec1, vec2, vec3, vec4) shouldBe 1.0
    }
    
    it should "work with identical vectors" in {
        sumCosines(vec1, vec1, vec2, vec2) shouldBe 2.0
    }
    
    it should "work with opposite vectors" in {
        val vecOpposite = Vector2D(-1, -1)
        sumCosines(vec3, vec3, vec3, vecOpposite) shouldBe 2.0 + (-1.0)
    }
    
    it should "handle zero vectors gracefully" in {
        val zeroVec = Vector2D(0, 0)
        sumCosines(vec1, zeroVec, vec2, zeroVec).isNaN shouldBe true
    }
}

    class BallsTest extends AnyFlatSpec with Matchers {
    
    "sortByHeavyweight" should "sort balls by mass correctly" in {
        val testBalls = Map(
            "SmallLight" -> (1, 1.0),    
            "SmallHeavy" -> (1, 10.0),  
            "BigLight" -> (2, 1.0),      
            "BigHeavy" -> (2, 10.0)      
        )
        
        val result = sortByHeavyweight(testBalls)
        result shouldBe Seq("SmallLight", "BigLight", "SmallHeavy", "BigHeavy")
    }
    
    it should "work with single ball" in {
        val singleBall = Map("Test" -> (5, 2.0))
        sortByHeavyweight(singleBall) shouldBe Seq("Test")
    }
    
    it should "work with empty map" in {
        sortByHeavyweight(Map.empty) shouldBe Seq()
    }
         it should "handle balls with same mass" in {
        val sameMassBalls = Map(
            "A" -> (2, 1.0),  
            "B" -> (1, 8.0)   
        )
        
        val result = sortByHeavyweight(sameMassBalls)
        result.toSet shouldBe Set("A", "B")
        result.length shouldBe 2
    }
    
    it should "use default parameter when no argument provided" in {
        val result = sortByHeavyweight()
        result should not be empty
        result.length shouldBe balls.size
    }
    
    it should "correctly calculate mass using volume formula" in {
        val testBall = Map("Test" -> (1, 1.0))
        val result = sortByHeavyweight(testBall)
        result shouldBe Seq("Test")
    }
}

