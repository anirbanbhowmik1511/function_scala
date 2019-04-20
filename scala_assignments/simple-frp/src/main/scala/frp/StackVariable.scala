package frp

class StackVariable[T](init : T) {
  var values = List(init)
  def value : T = values.head
  def withValue[R](v : T)( op : => R) : R = {
    values = v :: values
    try {
      op
    }finally{
      values = values.tail
    }
  }
}