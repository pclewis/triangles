# tris

![The basic concept](doc/intro.svg)

How many triangles can you find?

This app proves that the number of triangles that can be made inside of an equilateral triangle where `a` `b` and `c` are the number of internal lines coming from the corresponding point and no 3 of those lines cross at a single point is:

![1 + abc + ½(aab + baa + bcc + cbb + acc + caa + aa + bb + cc + 3a + 3b + 3c)](doc/formula.gif)

It does this by generating an svg showing every possible triangle.

## Screenshot

![screenshot](doc/screenshot.png)

## Explanation

Formula                        | Explanation 
-------------------------------|-------------------
1                              | The base triangle
abc                            | If 3 lines can't cross at a single point, they must cross at 3 points and make a triangle<br>![crossing at single point](doc/abc-bad.svg) ![making a triangle](doc/abc-good.svg) 
2a + 2b + 2c                   | Each line from `a` creates a point `xᵃ` on line `A`, making triangles `abxᵃ` and `acxᵃ`<br>![triangles on far side](doc/2a.svg)
T<sub>a-1</sub> + T<sub>b-1</sub> + T<sub>c-1</sub>| Every pair of lines from `a` creates a triangle with a segment of line `A` as the third line<br>![pairs](doc/pairs.svg)
g(a,b) + g(b,c) + g(a,c)       | Triangles created from intersections between two lines
**where** g(a,b) | =
3ab                            | Each line from `a` splits each line from `b` at point `y`, making triangles ayx<sup>b</sup>, byx<sup>a</sup>, and `ayb`. This is similar to `2a` above, but with a third triangle created using point x<sup>a</sup>.<br>![3ab](doc/3ab.svg)
T<sub>a-1</sub>b + T<sub>b-1</sub>a | Every pair of lines from `a` creates a triangle with each line from `b`, and vice versa. This is the same as the triangles created along the edges above.<br>![crosspairs](doc/crosspairs.svg)

## Observations

 - If c=0, it simplifies to ½(a+1)(b+1)(a+b+2)
 - If c=0 and a=b, it simplifies to (a+1)³
 - When treated as a graph with a node at each line crossing and an edge between each pair of nodes in a line:
   - The number of edges is a\*T(b+c+1)+T(a+1)+b\*T(a+c+1)+T(b+1)+c\*T(a+b+1)+T(c+1)
     - Where T is the [Triangle Number](https://en.wikipedia.org/wiki/Triangle_number), i.e. T(n)=n*(n+1)/2
   - The number of nodes is ab + bc + ac + a + b + c + 3
     - Every line from each side crosses every line from the other sides, and adds a node on the opposite base line. Plus 3 starting nodes from the base triangle.
   - If c=0, the number of triangles is edges-nodes+1
   
## Usage

`lein run` and visit `localhost:3000`

The four boxes are `a`, `b`, `c`, and `o`, where `o` is an offset to add to each angle to prevent 3 lines from crossing a single point.

## License

[![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)](http://creativecommons.org/publicdomain/zero/1.0/)

To the extent possible under law, the person who associated CC0 with this work has waived all copyright and related or neighboring rights to this work.
