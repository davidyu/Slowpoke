# gml-cpp
[![Build status](https://travis-ci.org/davidyu/gml-cpp.svg?branch=master)](https://travis-ci.org/davidyu/gml-cpp/builds)

gml (game math library) for C++11. Under heavy development, so usage in production code is discouraged.

### Design notes

The API and general design of this library is based on [Nathan Reed's On Vector Math Libraries article](http://www.reedbeta.com/blog/on-vector-math-libraries/).

This is a C++11 library. There are a few .cpp files that your compiler needs to be aware of (`matrix.cpp` and `collision.cpp`).
For the rest, just including the corresponding .h files should be enough (make sure your header include paths are properly
configured).
The vector and matrix classes are templatized, so you can define arbitrarily sized vectors and matrices
(though the generated assembly might be gnarly to examine as a result):

```
#include "vector.h"
#include "matrix.h"

int main() {
  Vec<2> v2 = { 1, 2 };
  Vec<6> v6 = { 1, 2, 3, 4, 5, 6 };
  
  Mat<2,3> m23 = { 11, 12, 13, 21, 22, 23 }; // produces a 2x3 matrix (2 rows, 3 columns)
  return 0;
}
```

Template specializations and aliases are provided for common vector and matrix sizes in games, such as `Vec2`, `Vec3`, `Vec4`, `Mat3` and `Mat4`:

```
#include "vector.h"
#include "matrix.h"
#include <iostream>

int main() {
  using namespace std;
  
  Vec2 v2 = { 1, 2 };
  cout << v2.x << " " << v2.y << "\n"; // -> 1 2
  
  Vec4 v4 = { 1, 2, 3, 4 };
  cout << v4.x << v4.y << v4.z << v4.w << "\n"; // -> 1234
  
  // a color is a Vec<4> typedef
  Color c = v4;
  cout << c.a << c.r << c.g << c.b << "\n"; // -> 4123
  
  Mat4 m = identity<4>(); // produces a 4x4 identity matrix
  cout << m.m00 << m.m11 << m.m22 << m.m33 << "\n"; // -> 1111
  return 0;
}
```

Matrices are stored in row-major order.

Common arithmetic operators (`+, -, *, /, ^, +=, -=, *=, /=, ^=, ==, !=`) are overloaded:

```
#include "vector.h"
#include "matrix.h"
#include <iostream>

int main() {
  using namespace std;
  
  Vec4 pos = { 0, 0, 0, 1 };
  
  Mat4 m = identity<4>();
  
  Vec4 result = m * pos; // -> { 0, 0, 0, 1 }
  
  m.tx = 1;
  m.ty = 2;
  m.tz = 3;
  
  result = pos * v4; // -> { 1, 2, 3, 1 }
  
  result -= { 1, 2, 3, 0 }; // -> { 0, 0, 0, 1 }
  
  return 0;
}
```
