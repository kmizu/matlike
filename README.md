# Matlike: A language for calculating matrices

![Build Status](https://github.com/kmizu/matlike/actions/workflows/ci.yml/badge.svg)

## Introduction

Matlike is an experimental language for calculating matrices.  Matlike have characteristics as follows:

- First-class matrices

Matrices is first-class citizen in Matlike.  For example, a matrix is described as follows:

```
val m = [
 1 2 3
 4 5 6
] // val m: Matrix<Int, 2, 3>
```

It is same as below.

```
val m = [
  1, 2, 3
  4, 5, 6
]
```

Note that any comma or any square bracket is not needed unlike other languages.

- Powerful matrix type inference as an extension of Hindley-Milner type inference

If you write as follows:

```
def mult(x, y) = x _*_ y
```

You'll get below.

```
def mult<A, B, C, D>(x: Matrix<A, B, C>, y: Matrix<A, C, D>): Matrix<A, B, D> = x _*_ y
```

You don't need to write extra type annotations mostly if you use not-overloaded operators.

Matlike is not for production purpose.  It is experimentation of type inference about matrix types.

## Build from sources

Since Matlike is not released yet, you'll need to build `matlike.jar` from sources.

Requirement:

- Java 8 or later
- sbt 1.3.0 or later

```sh
$ sbt assembly
...

$ find . -name "*.jar"
./target/scala-2.13/matlike.jar

$ java -jar target/scala-2.13/matlike.jar
> 1
value = 1
> 23
value = 23
> [1 2 3; 4 5 6]
value =
[
  1 2 3
  4 5 6
]

> [1 2; 3 4; 5 6] + 3
value =
[
  4 5
  6 7
  8 9
]

$ java -jar target/scala-2.13/matlike.jar -e "[1 2 3; 4 5 6]"

[
  1 2 3
  4 5 6
]
```
