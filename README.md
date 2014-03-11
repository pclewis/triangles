# tris

Prove that the number of triangles that can be made inside of an equilateral triangle with x lines projected from the left, y lines projected from the right, and z lines projected from the top, and no 3 lines crossing in the middle at a single point, is equal to:

1/2(xx+yy+zz + xxy+xxz + yyx+yyz + zzx+zzy + 4(xy+xz+yz) + 3(x+y+z) + 2xyz + 2)

## Observations

 - If z=0, it simplifies to 1/2(x+1)(y+1)(x+y+2)
 - If z=0 and x=y, it simplifies to (x+1)^3
 - When treated as a graph with a node at each line crossing and an edge between each pair of nodes in a line:
   - The number of edges is x*T(y+z+1)+T(x+1)+y*T(x+z+1)+T(y+1)+z*T(x+y+1)+T(z+1)
     - Where T is the [Triangle Number](https://en.wikipedia.org/wiki/Triangle_number), i.e. T(n)=n*(n+1)/2
   - The number of nodes is xy + xz + yz + 3xyz
   - If z=0, the number of triangles is edges-nodes+1

## License

[![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)](http://creativecommons.org/publicdomain/zero/1.0/)

To the extent possible under law, the person who associated CC0 with this work has waived all copyright and related or neighboring rights to this work.
