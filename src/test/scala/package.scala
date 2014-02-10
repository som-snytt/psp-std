package psp

package object tests extends psp.core.PackageTraits {
  lazy val RunNow = utest.ExecutionContext.RunNow
  val TestSuite = utest.framework.TestSuite
  type TestSuite = utest.framework.TestSuite
}
