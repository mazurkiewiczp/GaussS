import scala.io.Source
import java.io.FileNotFoundException
import java.io.IOException
import java.lang.ArrayIndexOutOfBoundsException
import util.Random
import math.abs

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

def printVector(X:Array[Double]) = {
	for (i <- 0 until X.length)
		println("X_" + (i+1) + " = " + X(i))	
}

def doTheThing(A:Array[Array[Double]],Y:Array[Double]) = {
	if (args.length < 2 || (args.length>1 && args(1) != "gs")){
		println("Gaussian Elimination (default)")
		val ans = gaussElimination(A,Y)
		printVector(ans)
	}
	else {
		println("Gauss-Seidel method")
		val ans = gaussSeidel(A,Y)
		printVector(ans)
	}
}

if (args.length == 0){
println("No arguments provided. Need file path.")
System.exit(0)
}

try {
	var data = Source.fromFile(args(0)).getLines().map(_.split("\t")).toList
	var A = Array.ofDim[Double](data.length,data(0).length-1)
	var Y = new Array[Double](data.length)
	for (i <- 0 until data.length){
		for (j <- 0 until data(0).length-1)
		A(i)(j)=data(i)(j).toDouble
	Y(i)=data(i)(data(0).length-1).toDouble
	}
	doTheThing(A,Y)
} catch {
  case ex: FileNotFoundException => println("Couldn't find that file.")
  case ex: IOException => println("Had an IOException trying to read that file.")
  case ex: ArrayIndexOutOfBoundsException => println("Data should have N rows and N+1 columns.")
}
