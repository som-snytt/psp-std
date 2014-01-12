Collections, especially views.

There are a few different approaches tried in here. Don't take anything too seriously.

It depends on a trunk build of scala master. 2.11.0-M7 isn't good enough. 2.11.0-M8 should be once it's available.

sbt test
--------

```
[info] No tests to run for root/test:test
[info] SizeInfoSpec
[info] + `+` on precises
[info] + s1 <= (s1 max s2)
[info] + s1 >= (s1 min s2)
[info] + s1 <= (s1 + s2)
[info] + s1 >= (s1 - s2)
[info] + <inf> + n
[info] + `+` is associative
[info] + `max` is associative
[info] + `min` is associative
[info] + `+` is commutative
[info] + `max` is commutative
[info] + `min` is commutative
[info]
[info] Total for specification SizeInfoSpec
[info] Finished in 550 ms
[info] 12 examples, 1200 expectations, 0 failure, 0 error
[info] InfiniteSpec
[info] + Basis collection is 1 to 100                        Psp  Stream  Strm/V
[info] + .                                                 ------  ------  ------
[info] + .   dropR    3,     take    1,   filter   /5          4       4   99999   //
[info] + .   slice  2/3,   filter   /6,  flatMap  **2          4       3       3   //
[info] + .    take    5,    dropR    1,   filter   /4          5       5       9   // 4
[info] + .   dropR    3,  flatMap  **1,      map   *5          8       8   99999   // 5, 10, 15, 20, 25
[info] + .   dropR    3,  collect  1**,    dropR    6         14      14   99999   // 0, 0, 1, 1, 1
[info] + . flatMap  **2,     drop   10,    dropR   13         14      14   99999   // 6, 6, 7, 7, 8
[info] + .  filter   /2,    dropR    8,  flatMap **10         18      18   99999   // 2, 2, 2, 2, 2
[info] + . collect  3**,    dropR    5,      map   *2         30      30   99999   // 2, 4, 6, 8, 10
[info] + .   dropR    4,  collect  9**,   filter   /1         49      49   99999   // 3, 6, 9, 12, 15
[info] + .    drop    2,    dropR    2,  collect 12**         62      62   99999   // 4, 8, 12, 16, 20
[info] + .   dropR    2,      map   *1,   filter  /12         62      62   99999   // 12, 24, 36, 48, 60
[info]
[info] Total for specification InfiniteSpec
[info] Finished in 3 ms
[info] 13 examples, 0 failure, 0 error
[info] PreciseSpec
[info] + Basis collection is 1 to 100                      Psp/L   Psp/I    List  List/V  Stream  Strm/V  Rnge/V  Vect/V
[info] + .                                                 ------  ------  ------  ------  ------  ------  ------  ------
[info] + .    take    8,    dropW  <21,     drop   20          0       0     100       8       8       8       8       8   //
[info] + .    take   16,     drop   12,    dropR   44          0       0     100      28      16      28      28      28   //
[info] + .   slice  3/2,    dropR   44,   filter  /17          0       0     100       6       1       6       6       6   //
[info] + .     map  *15,    takeW  <34,    takeR   62          3       3     100       6       3       6       6       6   // 15, 30
[info] + .   slice  4/9,  flatMap  **9,    takeW  <25          5       1     100       5       5       5       5       5   // 5, 5, 5, 5, 5
[info] + .   dropR    1,      map  *40,    slice  4/5          7       1     100     105       6     105     105     105   // 200
[info] + .   dropR    7,      map  *22,    takeW  <22          8       1     100     101       8     101     101     101   //
[info] + .   slice 3/25,      map  *46,      map  *36          8       5     100       8       8       8       8       8   // 6624, 8280, 9936, 11592, 13248
[info] + .    take   11,    takeR   40,  collect 13**         11      11     100      22      11      22      22      22   //
[info] + .   dropR   10,  flatMap **22,    takeW  <39         11       1     100     101      11     101     101     101   // 1, 1, 1, 1, 1
[info] + .   slice 4/14,  collect 48**,   filter  /46         15      10     100      14      14      14      14      14   //
[info] + .   slice 0/15,     drop    4,    takeR   55         16       5     100      24      15      24      24      24   // 5, 6, 7, 8, 9
[info] + .   slice 0/15,    takeR   10,  collect 42**         16      10     100      30      15      30      30      30   //
[info] + .    drop   16,  flatMap **47,    takeW  <65         17       1     100      17      17      17      17      17   // 17, 17, 17, 17, 17
[info] + .     map  *11,     drop   13,     take   60         18       5     100      18      18      18      18      18   // 154, 165, 176, 187, 198
[info] + .   slice 3/18,    takeR    4,    dropW  <47         19       4     100      36      18      36      36      36   //
[info] + .   takeW  <19,    takeR   23,  flatMap **56         19      19     100      20      19      20      20      20   // 1, 1, 1, 1, 1
[info] + .    drop   21,    takeW   <7,      map   *7         22       1     100      22      22      22      22      22   //
[info] + .    drop   18,     take   13,      map  *31         23       5     100      23      23      23      23      23   // 589, 620, 651, 682, 713
[info] + . collect  1**,     take   49,    dropR   22         27      27     100      54      27      54      54      54   // 0, 0, 1, 1, 1
[info] + .    drop    2,    dropR   28,  flatMap **51         31       1     100     103      31     103     103     103   // 3, 3, 3, 3, 3
[info] + .   dropR    9,   filter   /5,      map   *6         34      25     100     125      34     125     125     125   // 30, 60, 90, 120, 150
[info] + .   dropR   24,    takeW  <12,    takeR   72         36      12     100     117      36     117     117     117   // 1, 2, 3, 4, 5
[info] + .    drop    3,   filter  /19,   filter  /57        100      97     100     100     100     100     100     100   // 57
[info] + .    drop   11,    takeR   28,    takeR   15        100       5     100     290     100     290     290     290   // 86, 87, 88, 89, 90
[info] + .   dropR    2,     drop   24,  collect 56**        100      74     100     198     100     198     198     198   // 18
[info] + .   dropR   22,      map  *26,    takeR   39        100       5     100     222     100     222     222     222   // 1040, 1066, 1092, 1118, 1144
[info] + .   dropW  <10,   filter  /18,    takeR   64        100     100     100     190     100     190     190     190   // 18, 36, 54, 72, 90
[info] + .   dropW  <13,    dropR   43,    takeR   69        100     100     100     174     100     174     174     174   // 13, 14, 15, 16, 17
[info] + .   takeR    8,    dropR   42,      map  *15        100       0     100     292     100     292     292     292   //
[info] + .   takeR   13,    takeR   45,    takeW  <24        100       1     100     288     100     288     288     288   //
[info] + .   takeR   16,    takeR   26,    dropW  <22        100       5     100     289     100     289     289     289   // 85, 86, 87, 88, 89
[info] + .   takeR   20,  collect 39**,    slice 0/75        100      20     100     200     100     200     200     200   //
[info] + .  filter  /23,    takeR   42,    takeR   68        100     100     100     300     100     300     300     300   // 23, 46, 69, 92
[info] + . collect 23**,  collect 14**,    takeR   50        100     100     100     200     100     200     200     200   //
[info]
[info] Total for specification PreciseSpec
[info] Finished in 6 ms
[info] 37 examples, 0 failure, 0 error
[info] ViewSpec
[info] + Bitset expression 1 results in BitSet
[info] + Bitset expression 2 results in BitSet
[info] + Bitset expression 3 results in BitSet
[info] + Bitset expression 4 results in BitSet
[info] + String expression 1 results in String
[info] + String expression 2 results in String
[info] + Array[Int] expression 1 results in Array
[info] + Map expression 1 results in Map
[info] + Seq expression 1 results in Seq
[info]
[info] Total for specification ViewSpec
[info] Finished in 2 ms
[info] 9 examples, 0 failure, 0 error
[info] Passed: Total 71, Failed 0, Errors 0, Passed 71
[success] Total time: 8 s, completed Jan 12, 2014 5:49:17 AM
```