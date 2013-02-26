package delegado_test

import org.specs2.mutable._
import delegado._

// NOTE: these examples are taken (and adapted) from the documentation for the autoproxy plugin by Kevin wright.
class DelegadoTest extends Specification {
  sequential

  abstract class I {
    def foo(): String
    def bar(): String
    def bar(arg: String): String
    def baz(): String
  }

  "Delegado" should {
  
    "properly delagate to a @delegate val" in {
      class A extends I {
        def foo() = "a.foo"
        def bar() = "a.bar"
        def bar(arg: String) = "a.bar: " + arg
        def baz() = "a.baz"
      }
      
      abstract class B(@delegate val a : A) extends I {
        def bar() = "b.bar"
      }
      object B {
        implicit val delegateBuilder = Delegate.builder[B]
      }
      
      val a = new A

      val b = Delegate[B].build( a )
      b.foo() must_== "a.foo"
      b.bar() must_== "b.bar"
      b.bar("yop") must_== "a.bar: yop"
      b.baz() must_== "a.baz"

      // Same as above, but with alternate instantiation style
      val b2 = B.delegateBuilder( a )
      b2.foo() must_== "a.foo"
      b2.bar() must_== "b.bar"
      b2.bar("yop") must_== "a.bar: yop"
      b2.baz() must_== "a.baz"
    }
  
    "properly delagate to a @delegate object" in {
      abstract class C extends I {
        @delegate object Inner /*extends I*/ {
          def foo() = "inner.foo"
          def bar() = "inner.bar"
          def bar(arg: String) = "inner.bar: " + arg
          def baz() = "inner.baz"
        }
        def bar() = "c.bar"
      }
      object C {
        implicit val delegateBuilder = Delegate.builder[C]
      }
      
      val c = Delegate[C].build()
      c.foo() must_== "inner.foo"
      c.bar() must_== "c.bar"
      c.bar("yep") must_== "inner.bar: yep"
      c.baz() must_== "inner.baz"
    }
    
    "properly delegate to methods not declared explictly in the containing class, and make accessible" in {
      class Foo {
        @delegate protected object properties {
          var a: Int = 123
          var b: Double = 4.56
          var c: String = "hello"
        }      

        // The following defs are mixed in: a, a_=, b, b_=, c
        // a delegate for c_= is NOT created as it already exists below
        def c_=(str: String) = {
          properties.c = str + "!"
        }
      }
      object Foo {
        implicit val delegateBuilder = Delegate.builder[Foo]
      }

      // NOTE: we let type inference work here, and as a result foo has not type Foo,
      //       but the type of the actual instance returned by Delegate.builder[Foo], which
      //       has additional members (a, b, and c).
      val foo = Delegate[Foo].build()
      foo.a must_== 123
      foo.b must_== 4.56
      foo.c must_== "hello"
      foo.c = "bye"
      foo.c must_== "bye!"
    }
  }
}
