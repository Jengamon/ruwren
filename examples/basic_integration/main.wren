import "maths" for Math, Vector

var poke_vector = Fiber.new {
    Vector.invalid()
}

System.print("Hello World")
System.print(poke_vector.try())

var vector = Math.new_vector(3, 4)

System.print(vector)
System.print(vector.x)
System.print(vector.y)

vector.x = (10.2)
vector.y = (vector.x * 2)

System.print(vector.x)
System.print(vector.y)