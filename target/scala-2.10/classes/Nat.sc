/*
Natural number implementation
* */
//zero number
class Zero extends Nat{
  override def isZero: Boolean = true
  override def predecessor: Nat = throw new Error()
  override def - (that: Nat) = if(that.isZero) this else throw new Error("negative number")
}
//non-zero number
class Succ(n: Nat) extends Nat{
  override def isZero: Boolean = false
  override def predecessor: Nat = n
  override def -(that: Nat): Nat = if (that.isZero) this else predecessor - that.predecessor
}

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def +(that: Nat): Nat = if (that.isZero) this else successor + that.predecessor
  def -(that: Nat): Nat

  override def toString = {

    def createNumberFromNat(n : Nat) : Int = {
      if (n.isZero) 0
      else 1 + createNumberFromNat(n.predecessor)
    }

    createNumberFromNat(this).toString
  }
}


val n0 = new Zero()
val n1 = new Succ(n0)
val n2 = new Succ(n1)
val n3 = new Succ(n2)

n0
n1
n2
n3
n2 + n2
n3 + n2
n2 + n2
n2 + n3