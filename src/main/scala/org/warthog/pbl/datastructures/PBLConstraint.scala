package org.warthog.pbl.datastructures

/**
 *  Trait for a pseudo-boolean constraint
 */
trait PBLConstraint {
  val terms: List[PBLTerm]
  val degree: BigInt

  
}
