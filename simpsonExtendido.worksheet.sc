def simpson(a:Int, b:Int, f:Double => Double) : Double = {
    (b-a)*((f(a)+4*f((a+b)/2)+f(b))/6)
}

def simpsonCompuesta(a:Double, b:Double, n:Int, f:Double => Double) : Double = {
    val h = (b-a)/n
    val xj = (j:Double) => a + (j*h)

    val formula = (j:Double) => f(xj(2*j-2))+4*f(xj(2*j-1))+f(xj(2*j))
    (h/3) * (1 to (n/2)).map(formula(_)).sum
} 

def simpsonExtendida(a:Double, b:Double, f:Double => Double) : Double = {
    val n = 2 * (b-a)
    val h = (b-a)/n
    val s1 = (i:Double) => f(a+(i*h))
    val s2 = (j:Double) => f(a+(j*h))

    (h/3) * (f(a) + (4 * Range(1, n.toInt, 2).map(s1(_)).sum) + (2 * Range(2, (n-1).toInt, 2).map(s2(_)).sum) + f(b))
}
    
val nPar = 8

val f = (x : Double) => -Math.pow(x,2) + (8*x) -12
simpson(3, 5, f)
simpsonCompuesta(3, 5, nPar, f)
simpsonExtendida(3, 5, f)

val g = (x : Double) => 3*Math.pow(x,2)
simpson(0, 2, g)
simpsonCompuesta(0, 2, nPar, g)
simpsonExtendida(0, 2, g)

val h = (x : Double) => x + 2*Math.pow(x,2) - Math.pow(x,3) + 5*Math.pow(x,4)
simpson(-1,1,h)
simpsonCompuesta(-1, 1, nPar, h)
simpsonExtendida(-1,1,h)

val i = (x : Double) => ((2*x+1)/Math.pow(x,2) + x)
simpson(1, 2, i)
simpsonCompuesta(1, 2, nPar, i)
simpsonExtendida(1, 2, i)

val j = (x : Double) => Math.pow(Math.E, x)
simpson(0, 1, j)
simpsonCompuesta(0, 1, nPar, j)
simpsonExtendida(0, 1, j)

val k = (x : Double) => (1/Math.sqrt(x-1))
simpson(2, 3, k)
simpsonCompuesta(2, 3, nPar, k)
simpsonExtendida(2, 3, k)

val l = (x : Double) => (1/1+Math.pow(x,2))
simpson(0, 1, l)
simpsonCompuesta(0, 1, nPar, l)
simpsonExtendida(0, 1, l)

