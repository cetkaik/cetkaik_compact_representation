# cetkaik_compact_representation
机戦の盤面をコンパクトに格納する

## 格納方法

まず、以下を「駒番号」とする。

![](piece_number.png)

### Board
Board は、9 × 9 の二次元配列に、以下の順序で駒情報を詰めたものであり、81 バイトで構成されてアラインメントは 1 である。

```
[
  [KA, LA, ..., PA],
  [KE, LE, ..., PE],
  ...
  [KIA, LIA, ..., PIA]
]
```

- 上位2bitが 0b00 → どちらも動かせない（空きマス）
- 上位2bitが 0b01 → IASideが動かせる 
- 上位2bitが 0b10 → ASideが動かせる 
- 上位2bitが 0b11 → どちらも動かせる（皇）

### Field
Field は、Board に手駒情報を加えたものであり、手駒情報は 96 ビット（= 12 バイト）のビットベクトルで表される。アラインメントは 1 である。
