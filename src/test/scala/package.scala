package psp

package object tests extends psp.core.PackageTraitsExceptShadowing with psp.core.PspShadowRequired {
  lazy val RunNow = utest.ExecutionContext.RunNow
  type TestSuite = utest.framework.TestSuite
}
