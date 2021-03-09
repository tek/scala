import scala.tools.partest._

object Test
extends DirectTest
{
  override def extraSettings: String = "-usejavacp -Vimplicits no-color"

  def code: String = ""

  def chain: String = """
object ImplicitChain
{
  trait I1
  trait I2
  trait I3
  trait I4
  trait II
  implicit def i1(implicit impPar7: I3): I1 = ???
  implicit def i2a(implicit impPar8: I3): I2 = ???
  implicit def i2b(implicit impPar8: I3): I2 = ???
  implicit def i4(implicit impPar9: I2): I4 = ???
  implicit def g(implicit impPar3: I1, impPar1: I4): II = ???
  implicitly[II]
}
  """

  def foundReq: String = """
object FoundReq
{
  class L
  type R
  def f(r: R): Int = ???
  f(new L)
}
  """

  def bounds: String = """
object Bounds
{
  trait Base
  trait Arg
  trait F[A]
  implicit def g[A <: Base, B]: F[A] = ???
  implicitly[F[Arg]]
}
  """

  def longAnnotationMessage: String = """
object Long
{
  def long(implicit ec: concurrent.ExecutionContext): Unit = ???
  long
}
  """

  def longInfix: String = """
object InfixBreak
{
  type ::::[A, B]
  trait VeryLongTypeName
  trait Short
  type T1 = VeryLongTypeName :::: VeryLongTypeName :::: VeryLongTypeName ::::
    VeryLongTypeName
  type T2 = T1 :::: (Short :::: Short) :::: T1 :::: T1
  implicit def f(implicit impPar4: List[T2]): String = ???
  implicitly[String]
}
  """

  def show(): Unit = {
    val global = newCompiler()

    def run(code: String): Unit =
      compileString(global)(code.trim)

    run(chain)
    run(foundReq)
    run(bounds)
    run(longAnnotationMessage)
    run(longInfix)
  }
}
