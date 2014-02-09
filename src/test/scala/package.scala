package psp

package object tests extends psp.core.PackageTraitsExceptShadowing with psp.core.PspShadowRequired {
  lazy val RunNow = utest.ExecutionContext.RunNow
  val TestSuite = utest.framework.TestSuite
  type TestSuite = utest.framework.TestSuite
}
