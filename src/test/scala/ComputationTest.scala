import com.rishabh.hw5.Computation.*
import com.rishabh.hw5.Computation.SetExp.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Map, Set}

class ComputationTest extends AnyFlatSpec with Matchers{

  // Test 1
  it should "check for partial and complete evaluation" in {
    assert(Variable("x").eval() === Variable("x"))

    Assign("x", Value(Set(1))).eval()
    assert(Variable("x").eval() === mutable.HashSet(1))

    Assign("x", Value(Set(1))).eval()
    assert(Union(Variable("x"), Variable("Y")).eval() === Union(Value(mutable.HashSet(1)),Variable("Y")))
  }

  // Test 2
  it should "check monadic function for Union" in {

    def func(input: SetExp) = {
      input match {
        case Value(x) => Value(x)
        case Variable(x) => {
          val output = Variable(x).eval()
          val re = output match {
            case p: PartialResult => Variable(x)
            case a: Any => Value(a)
          }
          re
        }

        // Optimizations
        // 1. Union(x, x) => Value(x)
        // 2. Union(x, emptySet) => Value(x)
        case Union(x, y) => {
          if (x == y || y.eval() == mutable.HashSet())
            x.eval()
          else if (x.eval() == mutable.HashSet())
            y.eval()
          else
            Union(x, y).eval()
        }
      }
    }

    Assign("x", Value(Set(1))).eval()
    // Value of x is present
    assert(MonadsOptimize(Union(Variable("x"), Variable("x"))).map(func) === mutable.HashSet(1))

    // Value of a is not present
    assert(MonadsOptimize(Union(Variable("a"), Variable("a"))).map(func) === Variable("a"))
  }

  // Test 3
  it should "check monadic function for Diff" in {

    def func(input: SetExp) = {
      input match {
        case Value(x) => Value(x)
        case Variable(x) => {
          val output = Variable(x).eval()
          val re = output match {
            case p: PartialResult => Variable(x)
            case a: Any => Value(a)
          }
          re
        }

        // Optimizations
        // 1. Diff(x, x) => emptySet
        // 2. Diff(x, emptySet) => Value(x)
        case Diff(x, y) => {
          if(x == y)
            mutable.HashSet()
          else if(x.eval() == mutable.HashSet())
            y.eval()
          else if(y.eval() == mutable.HashSet())
            x.eval()
          else
            Diff(x, y).eval()
        }
      }
    }

    Assign("x", Value(Set(1,2,3,4))).eval()
    // Value of x is present
    assert(MonadsOptimize(Diff(Value(mutable.HashSet()), Variable("x"))).map(func) === mutable.HashSet(1, 2, 3, 4))

    // Value of a is not present
    assert(MonadsOptimize(Diff(Value(mutable.HashSet()), Variable("a"))).map(func) === Variable("a"))
  }

  // Test 4
  it should "check monadic function for Intersect for complete evaluation" in {

    def func(input: SetExp) = {
      input match {
        case Value(x) => Value(x)
        case Variable(x) => {
          val output = Variable(x).eval()
          val re = output match {
            case p: PartialResult => Variable(x)
            case a: Any => Value(a)
          }
          re
        }

        // Optimizations
        // 1. Intersect(x, x) => Value(x)
        // 2. Intersect(x, emptySet) => emptySet
        case Intersect(x, y) => {
          if(x == y)
            x.eval()
          else if(x.eval() == mutable.HashSet() || y.eval() == mutable.HashSet())
            mutable.HashSet()
          else
            Intersect(x, y).eval()
        }
      }
    }

    Assign("x", Value(Set(1))).eval()
    // Value of x is present
    assert(MonadsOptimize(Intersect(Variable("x"), Variable("x"))).map(func) === mutable.HashSet(1))
    assert(MonadsOptimize(Intersect(Variable("x"), Value(mutable.HashSet()))).map(func) === mutable.HashSet())
  }

  // Test 5
  it should "check monadic function for Intersect for partial evaluation" in {
    def func(input: SetExp) = {
      input match {
        case Value(x) => Value(x)
        case Variable(x) => {
          val output = Variable(x).eval()
          val re = output match {
            case p: PartialResult => Variable(x)
            case a: Any => Value(a)
          }
          re
        }

        // Optimizations
        // 1. Intersect(x, x) => Value(x)
        // 2. Intersect(x, emptySet) => emptySet
        case Intersect(x, y) => {
          if(x == y)
            x.eval()
          else if(x.eval() == mutable.HashSet() || y.eval() == mutable.HashSet())
            mutable.HashSet()
          else
            Intersect(x, y).eval()
        }
      }
    }

    // Value of a is not present
    assert(MonadsOptimize(Intersect(Variable("a"), Variable("a"))).map(func) === Variable("a"))
    assert(MonadsOptimize(Intersect(Variable("a"), Value(mutable.HashSet()))).map(func) === mutable.HashSet())
  }
}
