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
}
