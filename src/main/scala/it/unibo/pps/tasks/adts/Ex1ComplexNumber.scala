package it.unibo.pps.tasks.adts

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    // Change assignment below: should probably define a case class and use it?
    //type Complex = Double
    case class Complex(re: Double, im: Double)
    def complex(re: Double, im: Double): Complex = Complex(re, im)
    extension (complex: Complex)
      def re(): Double = complex.re
      def im(): Double = complex.im
      def sum(other: Complex): Complex = Complex(complex.re + other.re, complex.im + other.im)
      def subtract(other: Complex): Complex = Complex(complex.re - other.re, complex.im - other.im)
      def asString(): String = complex match
        case Complex(0, 0) => "0.0"
        case Complex(re, 0) => re.toString
        case Complex(0, im) => s"$im" + "i"
        case Complex(re, im) =>
          val sign = if im > 0 then "+" else "-"
          re.toString + s" $sign " + s"${im.abs}" + "i"


