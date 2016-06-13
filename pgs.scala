import scala.io.Source
import scala.collection.parallel.mutable
import java.io.FileNotFoundException
import java.io.IOException
import java.lang.ArrayIndexOutOfBoundsException
import java.lang.NumberFormatException
import util.Random
import math.abs


class met{
def gaussElimination(A:Array[Array[Double]],Y:Array[Double]):Array[Double] = {
	for(i <- 0 until A.length){
		for(j <- i+1 until A.length){
			var ratio = A(j)(i)/A(i)(i)
			A(j)(i) = 0.0			
			for(k <- i+1 until A.length){
				A(j)(k) -= ratio*A(i)(k)
			}
			Y(j) -= ratio*Y(i)
		}
	}
	var X = new Array[Double](A.length)
	for(i <- A.length-1 to 0 by -1){
		X(i) = Y(i)
		for(j <- i+1 until A.length) X(i) -= A(i)(j)*X(j)
		X(i) /= A(i)(i)
	}
	X
}

def gaussSeidel(A:Array[Array[Double]],Y:Array[Double]):Array[Double] = {
	var X = new Array[Double](A.length)
	var P = new Array[Double](A.length)
	var d = A.length
	val MAX = 100				
	var count = 0 
	val EPS = 0.000001			
	var stop = false
	var q = 0.0

	do
	{
	count += 1

	for (i <- 0 until A.length){
		q = 0.0
		for (j <- 0 until A.length) if(i!=j) q += A(i)(j)*P(j)
		X(i) = (Y(i)-q)/A(i)(i)
		}

	d = A.length
	for (k <- 0 until X.length){
		if (abs(P(k)-X(k))<EPS) d-=1
		P(k) = X(k)
		}
	if (d==0) stop = true

	} while (!stop && count <= MAX)
	X
}
}

def printVector(X:Array[Double]) = {
	for (i <- 0 until X.length)
		println("X_" + (i+1) + " = " + X(i))	
}

object parallelGauss{
def main() = {
	var ps = new met()
	spg(args(1).toInt)
	
	if (args.length == 0){
	println("No arguments provided. Need file path.")
	System.exit(0)
	}

	/*
	var mm  = Array.ofDim[Double](args(1).toInt , args(1).toInt )
	var mv  = Array.ofDim[Double](args(1).toInt)
	var rn = new scala.util.Random
	for( i <- (0 until mm.length).par; j<- (0 until mm(0).length).par)
	{
		mm(i)(j) =  rn.nextInt(100)
		mv(i) =rn.nextInt(100)
	}
	*/

	try {
	var data = Source.fromFile(args(0)).getLines().map(_.split("\t")).toList
	var A = Array.ofDim[Double](data.length,data(0).length-1)
	var Y = new Array[Double](data.length)
	for (i <- 0 until data.length){
		for (j <- 0 until data(0).length-1)
		A(i)(j)=data(i)(j).toDouble
	Y(i)=data(i)(data(0).length-1).toDouble
	}

	if (args.length < 3 || (args.length>1 && args(2) != "gs")){
		println("Gaussian Elimination (default)")
		val ans = ps.gaussElimination(A,Y)
		printVector(ans)
	}
	else {
		println("Gauss-Seidel method")
		val ans = ps.gaussSeidel(A,Y)
		printVector(ans)
	}
	
	} catch {
  	case ex: FileNotFoundException => println("Couldn't find that file.")
  	case ex: IOException => println("Had an IOException trying to read that file.")
  	case ex: ArrayIndexOutOfBoundsException => println("Data should have N rows and N+1 columns.")
  	case ex: NumberFormatException => println("Values should be tab-separated.")
	}	
}

def spg(numThreads: Int): Unit = {
val parPkgObj = scala.collection.parallel.`package`
val defaultTaskSupportField = parPkgObj.getClass.getDeclaredFields.find{
	_.getName == "defaultTaskSupport"
}.get
defaultTaskSupportField.setAccessible(true)
defaultTaskSupportField.set(
		parPkgObj, 
		new scala.collection.parallel.ForkJoinTaskSupport(
			new scala.concurrent.forkjoin.ForkJoinPool(numThreads)
					) 
			)
}
}

parallelGauss.main()
