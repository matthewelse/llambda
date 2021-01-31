# Figure out how cmm works

- blocks: 1 - caml=? tag=0 -> list/record/tuple, length 1 [ None ] ??? None
  means unit (=0 : int = 1 at runtime) apparently

- block_header = size << 10 + tag 

- caml_black = 3 << 8 = 768
- tag = 0, length = 1


```
a = 2n + 1
b = 2m + 1

a + b = 2n + 2m + 1 + 1
      = 2 (n + m) + 2

we want

2 (n + m ) + 1

so (a + b - 1)
```