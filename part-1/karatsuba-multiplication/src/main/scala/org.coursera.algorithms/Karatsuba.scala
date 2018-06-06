package org.coursera.algorithms

object Karatsuba {
  def main(args: Array[String]): Unit ={
    val inputA = BigInt("3141592653589793238462643383279502884197169399375105820974944592")
    val inputB = BigInt("2718281828459045235360287471352662497757247093699959574966967627")
    println(karatsuba(inputA, inputB))
  }

  def karatsuba(x: BigInt, y: BigInt): BigInt = {
    if(len(x) != len(y))println(s"x_length: ${len(x)}, y_length: ${len(y)}")

    val length = math.max(len(x).toInt, len(y).toInt)
    //base case
    if (length == 1) return x * y

    val a: BigInt = y / pow(BigInt(10), length / 2)
    val b: BigInt = y - a * pow(BigInt(10), length / 2)
    val c: BigInt = x / pow(BigInt(10), length / 2)
    val d: BigInt = x - c * pow(BigInt(10), length / 2)

    val ac: BigInt = karatsuba(a, c)
    val bd: BigInt = karatsuba(b, d)
    val adPlusBc: BigInt = karatsuba(a, d) + karatsuba(b, c)

    return pow(10, length) * ac + bd + pow(10, length / 2) * adPlusBc
  }

  def pow(x: BigInt, i: Int = 0): BigInt = {
    if(i == 0) 1
    else x * pow(x, i - 1)
  }

  def len(x: BigInt, i: Int = 1): BigInt =
    if (x < 10) i
    else len(x / 10, i + 1)
}
