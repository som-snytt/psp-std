package psp

package object tests {
  lazy val RunNow = utest.ExecutionContext.RunNow
  val TestSuite = utest.framework.TestSuite
  type TestSuite = utest.framework.TestSuite
}
