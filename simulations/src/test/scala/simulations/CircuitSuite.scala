package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  //
  // to complete with tests for orGate, demux, ...
  //
  
  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "or 1")
    
    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 2")
    
    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 3")
  }

  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "or 1")
    
    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 2")
    
    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 3")
  }
  
  test("demux no control example") {
    val in, out = new Wire
    in.setSignal(false)
    demux(in, List(), List(out))
    run
    assert(out.getSignal === false, "and 1")
    
    in.setSignal(true)
    demux(in, List(), List(out))
    run
    assert(out.getSignal === true, "and 2")
  }
  
  test("demux 1 control example") {
    println("demux 1 control example")
    val in, c, out1, out2 = new Wire
    val outList = List(out2, out1)
    
    in.setSignal(false)
    c.setSignal(false)
    demux(in, List(c), outList)
    run
    assert(out2.getSignal === false, "demux 1")
    assert(out1.getSignal === false, "demux 2")
    
    in.setSignal(false)
    c.setSignal(false)
    demux(in, List(c), outList)
    run
    assert(out2.getSignal === false, "demux 3")
    assert(out1.getSignal === false, "demux 4")

    println(outList)
    in.setSignal(true)
    c.setSignal(false)
    
    demux(in, List(c), outList)
    run
    printList(outList)
    assert(out2.getSignal === false, "demux 5")
    assert(out1.getSignal === true, "demux 6")
    
    in.setSignal(true)
    c.setSignal(true)
    demux(in, List(c), outList)
    run    
    printList(outList)
    assert(out2.getSignal === true, "demux 7")
    assert(out1.getSignal === false, "demux 8")
  }
  
  test("demux 2 control example") {
    val in, c2, c1, out1, out2, out3, out4 = new Wire
    val cList = List(c2, c1)
    val outList = List(out4, out3, out2, out1)
    
    in.setSignal(false)
    c2.setSignal(false)
    c1.setSignal(false)
    demux(in, cList, outList)
    run
    assert(out4.getSignal === false, "demux 1")
    assert(out3.getSignal === false, "demux 2")
    assert(out2.getSignal === false, "demux 3")
    assert(out1.getSignal === false, "demux 4")
    
    in.setSignal(false)
    c2.setSignal(false)
    c1.setSignal(true)
    demux(in, cList, outList)
    run
    assert(out4.getSignal === false, "demux 5")
    assert(out3.getSignal === false, "demux 6")
    assert(out2.getSignal === false, "demux 7")
    assert(out1.getSignal === false, "demux 8")
    
    in.setSignal(false)
    c2.setSignal(true)
    c1.setSignal(false)
    demux(in, cList, outList)
    run
    assert(out4.getSignal === false, "demux 9")
    assert(out3.getSignal === false, "demux 10")
    assert(out2.getSignal === false, "demux 11")
    assert(out1.getSignal === false, "demux 12")
    
    in.setSignal(false)
    c2.setSignal(true)
    c1.setSignal(true)
    demux(in, cList, outList)
    run
    assert(out4.getSignal === false, "demux 13")
    assert(out3.getSignal === false, "demux 14")
    assert(out2.getSignal === false, "demux 15")
    assert(out1.getSignal === false, "demux 16")
    
    in.setSignal(true)
    c2.setSignal(false)
    c1.setSignal(false)
    demux(in, cList, outList)
    run
    assert(out4.getSignal === false, "demux 17")
    assert(out3.getSignal === false, "demux 18")
    assert(out2.getSignal === false, "demux 19")
    assert(out1.getSignal === true, "demux 20")
    
    in.setSignal(true)
    c2.setSignal(false)
    c1.setSignal(true)
    demux(in, cList, outList)
    run
    assert(out4.getSignal === false, "demux 21")
    assert(out3.getSignal === false, "demux 22")
    assert(out2.getSignal === true, "demux 23")
    assert(out1.getSignal === false, "demux 24")
    
    in.setSignal(true)
    c2.setSignal(true)
    c1.setSignal(false)
    demux(in, cList, outList)
    run
    assert(out4.getSignal === false, "demux 25")
    assert(out3.getSignal === true, "demux 26")
    assert(out2.getSignal === false, "demux 27")
    assert(out1.getSignal === false, "demux 28")
    
    in.setSignal(true)
    c2.setSignal(true)
    c1.setSignal(true)
    demux(in, cList, outList)
    run
    assert(out4.getSignal === true, "demux 29")
    assert(out3.getSignal === false, "demux 30")
    assert(out2.getSignal === false, "demux 31")
    assert(out1.getSignal === false, "demux 32")
  }
  
  def printList(l: List[Wire]) {
    l.map(w => println(w.getSignal))
  }
}
