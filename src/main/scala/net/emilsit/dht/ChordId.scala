package net.emilsit.dht

/** ChordId defines the valid identifier space for keys/nodes.
  * This is constrained to be an integer between 0 and
  * 2^MAX_BITS^ inclusive.
  *
  * @param id the numeric value for the ChordId
  * @throws IllegalArgumentException if id is out of range
  */
final class ChordId(private val id: BigInt) {
  require(repInvariant, "id out of bounds: " +
    id.toString(16) + " not in [0," + ChordId.MAX_ID.toString(16) + "]")

  def repInvariant = (id >= 0) && (id <= ChordId.MAX_ID)

  // We don't actually want to add ChordIds together.
  // Typically we want to check +/- 1 relative to a given id.
  def +(that: BigInt) =
    new ChordId((id + that) % ChordId.MODULUS)

  def -(that: BigInt) =
    // Ensure that we compute a positive value for constructor
    new ChordId((ChordId.MODULUS + id - that) % ChordId.MODULUS)

  /*
   * Delegate core functionality to id
   */
  override def toString = id.toString(16)
  override def equals(other: Any): Boolean =
    other match {
      case other: ChordId => id.equals(other.id)
      case _ => false
    }
  override def hashCode(): Int = id.hashCode()
}

object ChordId {
  val MAX_BITS = 160
  private val MODULUS = BigInt(1) << MAX_BITS
  val MAX_ID = MODULUS - 1
  val MAX = ChordId(MAX_ID)

  def apply(i: Int)  = new ChordId(BigInt(i))
  def apply(i: Long) = new ChordId(BigInt(i))
  def apply(i: BigInt) = new ChordId(i)
}
