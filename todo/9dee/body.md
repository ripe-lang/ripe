newtype Celsius = f32
newtype Fahrenheit = f32

func main() i32 {
  var hot: Fahrenheit = 212.0 as Fahrenheit
  var cold: Celsius = hot as Celsius
  return (cold as f32) as i32
}
