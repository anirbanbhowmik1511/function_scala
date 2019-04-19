package frp

import scala.util.DynamicVariable

class Signal[T](expr : => T) {
  import Signal._
  private var myExpr : () => T = _
  private var myValue : T = _
  private var observers : Set[Signal[_]] = Set()
  update(expr)
  protected def update(exp : => T) : Unit = {
    myExpr = () => exp
    computeValue()
  }
  protected def computeValue() : Unit = {
    val newValue = caller.withValue(this)(myExpr())
    if(newValue != myValue){
      myValue = newValue
      val obs = observers
      observers = Set()
      obs.foreach(_.computeValue())
    }
  }
  
  def apply() : T = {
    observers = observers + caller.value
    assert(!caller.value.observers.contains(this), "cyclic signal definition")
    myValue
  }
}

object Signal{
  val caller = new DynamicVariable[Signal[_]](NoSignal)
  def apply[T](expr : => T) = new Signal(expr)
}

object NoSignal extends Signal[Nothing](???) {
  override def computeValue() = ()
}