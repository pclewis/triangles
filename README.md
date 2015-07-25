# tris

Prove that the number of triangles that can be made inside of an equilateral triangle with x lines projected from the left, y lines projected from the right, and z lines projected from the top, and no 3 lines crossing in the middle at a single point, is equal to:

1/2(xxy+xxz + yyx+yyz + zzx+zzy + xx+3x + yy+3y + zz+3z)) + 2(xy+xz+yz) + xyz + 1 
 - 1 for the base triangle
 - xyz because if they can't intersect as a single point, they must form a triangle
 - 1/2(xx + 3x) are the triangles created just from x and the base points (2, 5, 9, ...)
 - 1/2(xxy + yyx) + 2xy are the triangles created from x and y intersections

## Observations

 - If z=0, it simplifies to 1/2(x+1)(y+1)(x+y+2)
 - If z=0 and x=y, it simplifies to (x+1)^3
 - When treated as a graph with a node at each line crossing and an edge between each pair of nodes in a line:
   - The number of edges is x\*T(y+z+1)+T(x+1)+y\*T(x+z+1)+T(y+1)+z\*T(x+y+1)+T(z+1)
     - Where T is the [Triangle Number](https://en.wikipedia.org/wiki/Triangle_number), i.e. T(n)=n*(n+1)/2
   - The number of nodes is xy + xz + yz + x + y + z + 3
     - Every x crosses every y and z, every y crosses every z, each x y and z adds a node on the opposite base line, and there are 3 starting nodes
   - If z=0, the number of triangles is edges-nodes+1

## License

[![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)](http://creativecommons.org/publicdomain/zero/1.0/)

To the extent possible under law, the person who associated CC0 with this work has waived all copyright and related or neighboring rights to this work.
