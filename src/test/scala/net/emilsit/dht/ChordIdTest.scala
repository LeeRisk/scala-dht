package net.emilsit.dht

import java.security.MessageDigest
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test


class ChordIdTest extends AssertionsForJUnit {
  @Test
  def one() {
    assertNotNull(ChordId(1))
  }

  @Test
  def interestingConstructors() {
    val w = ChordId(BigInt(1) << 130)
    assertNotNull(w)

    val md: MessageDigest = java.security.MessageDigest.getInstance("SHA-1")
    val x = ChordId(BigInt(md.digest("hello world".toCharArray map(_.toByte))))
    assertNotNull(x)
  }

  @Test
  def negativeConstructionFails() {
    intercept[IllegalArgumentException] {
      ChordId(BigInt(-1))
    }
  }

  @Test
  def bigConstructionFails() {
    intercept[IllegalArgumentException] {
      ChordId(BigInt(1) << ChordId.MAX_BITS + 1)
    }
  }

  @Test
  def equalsNonNull() {
    val a = ChordId(1)
    assertTrue(a.equals(ChordId(1)))
  }

  @Test
  def equalsNull() {
    val a = ChordId(1)
    assertFalse(a.equals(null))
  }

  @Test
  def basicAddition() {
    val a = ChordId(1)
    assertEquals(ChordId(3), a + 2)
  }

  @Test
  def basicSubtraction() {
    val b = ChordId(2)
    assertEquals(ChordId(1), b - 1)
  }

  @Test
  def wrapAroundAdditionFence() {
    val a = ChordId(1)
    assertEquals(ChordId(0), a + ChordId.MAX_ID)
  }

  @Test
  def wrapAroundSubtractionFence() {
    assertEquals(ChordId.MAX, ChordId(0) - 1)
  }

  @Test
  def wrapAroundAdditionBig() {
    assertEquals(ChordId.MAX - 1, ChordId.MAX + ChordId.MAX_ID)
  }

  @Test
  def wrapAroundSubtractionBig() {
    assertEquals(ChordId(1), ChordId(0) - ChordId.MAX_ID)
  }

  @Test
  def between() {
    implicit def int2ChordId(i: Int) = ChordId(i)
    implicit def bigInt2ChordId(i: BigInt) = ChordId(i)
    case class BetweenTestCase(desc: String, outLow: ChordId, in: ChordId, outHigh: ChordId, lowBound: ChordId, highBound: ChordId)
    val testCases = List(
      BetweenTestCase("non-spanning", 0, 5, 11, 1, 10),
      BetweenTestCase("spanning", ChordId.MAX_ID - 1, 0, 11, ChordId.MAX_ID, 10)
    )

    for (tc <- testCases) {
      import tc._
      assertTrue (desc + " between in",        in        between(lowBound, highBound))
      assertFalse(desc + " between outLow",    outLow    between(lowBound, highBound))
      assertFalse(desc + " between outHigh",   outHigh   between(lowBound, highBound))
      assertFalse(desc + " between lowBound",  lowBound  between(lowBound, highBound))
      assertFalse(desc + " between highBound", highBound between(lowBound, highBound))

      assertTrue (desc + " betweenLeftIncl in",        in        betweenLeftIncl(lowBound, highBound))
      assertFalse(desc + " betweenLeftIncl outLow",    outLow    betweenLeftIncl(lowBound, highBound))
      assertFalse(desc + " betweenLeftIncl outHigh",   outHigh   betweenLeftIncl(lowBound, highBound))
      assertTrue (desc + " betweenLeftIncl lowBound",  lowBound  betweenLeftIncl(lowBound, highBound))
      assertFalse(desc + " betweenLeftIncl highBound", highBound betweenLeftIncl(lowBound, highBound))

      assertTrue (desc + " betweenRightIncl in",        in        betweenRightIncl(lowBound, highBound))
      assertFalse(desc + " betweenRightIncl outLow",    outLow    betweenRightIncl(lowBound, highBound))
      assertFalse(desc + " betweenRightIncl outHigh",   outHigh   betweenRightIncl(lowBound, highBound))
      assertFalse(desc + " betweenRightIncl lowBound",  lowBound  betweenRightIncl(lowBound, highBound))
      assertTrue (desc + " betweenRightIncl highBound", highBound betweenRightIncl(lowBound, highBound))

      assertTrue (desc + " betweenBothIncl in",        in        betweenBothIncl(lowBound, highBound))
      assertFalse(desc + " betweenBothIncl outLow",    outLow    betweenBothIncl(lowBound, highBound))
      assertFalse(desc + " betweenBothIncl outHigh",   outHigh   betweenBothIncl(lowBound, highBound))
      assertTrue (desc + " betweenBothIncl lowBound",  lowBound  betweenBothIncl(lowBound, highBound))
      assertTrue (desc + " betweenBothIncl highBound", highBound betweenBothIncl(lowBound, highBound))
    }
  }
}
