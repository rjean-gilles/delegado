package delegado_test

import org.specs2.mutable._
import delegado._

// NOTE: Some of these examples are taken (and adapted) from the documentation for the autoproxy plugin by Kevin Wright.
class DelegadoTest extends Specification {
  sequential
  
  // --------------------------------------------------
    
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
      
      implicit val myMapping = Delegate.Mapping{ x: A => null.asInstanceOf[B] }//TEST

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
    
    "properly delegate to methods not declared explictly in the containing class's bas traits/classes" in {
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
    // --------------------------------------------------
    "allow to delegate in a recursive way" in {
      // FIXME: ouch, this is ugly! Plus there is no actual "UInt" type that contains the operators "*","/" and so on
      
      class UInt /*private*/ ( @delegate(recursive=true) val i: Int ) {
        override def toString = i.toString
        override def hashCode = i
        override def equals( o: Any ) = o match {
          case ui2: UInt => ui2.i == i
          case _ => false
        }
      }
      object UInt {
        def apply( i: Int ): UInt = {
          require( i >= 0 )
          new UInt( i )
        }
        private implicit val toInt = Delegate.Mapping{ u: UInt => u.i }
        private implicit val fromInt: Delegate.Mapping[Int, UInt] = Delegate.Mapping{ i: Int => delegateBuilder( i ) }
        implicit lazy val delegateBuilder = Delegate.builder[UInt]
      }
            
      val u1 = Delegate[UInt].build( 3 )
      u1 must beAnInstanceOf[UInt]
      val u2 = u1 * Delegate[UInt].build( 2 )
      u2 must beAnInstanceOf[UInt]
      u2 must_== Delegate[UInt].build( 6 )
      /* FIXME: cannot actually work as is: u3 is of type UInt instead of the generate sub-type that contains the operators "*","/" and so on
      val u3 = u2 / Delegate[UInt].build( 3 )
      u3 must beAnInstanceOf[UInt]
      u3 must_== Delegate[UInt].build( 2 )
      */
    }
  }
}
