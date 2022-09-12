//Q1 and Q2

case class RationalNumber(p: Int, q: Int){
    if(q == 0) throw new IllegalArgumentException("Denominator cannot be 0")
    
    def this(p: Int) = this(p, 1)

    private def GCD(a: Int, b: Int): Int = if (b == 0) a.abs else GCD(b, (a % b))
    
    private def gcd = GCD(q, p)
    private def numer = p / gcd
    private def denom = q / gcd
    def neg =RationalNumber(-numer,denom)
    
    def -(that: RationalNumber) = RationalNumber(numer * that.denom - that.numer * denom, denom * that.denom)
    override def toString = s"$numer/$denom"
    
}

object RationalNumber extends App{
        val x = new RationalNumber(3, 4)
        val y = new RationalNumber(5, 8)
        val z = new RationalNumber(2, 7)

        println(x - y - z)
        println(x.neg)

}