\ F0 - Tw 1509888510525857792
: d [ 8000 8008 ] ;
: b [ 4 2 1 8 6 ] ;
: l { freq } freq 10 28 2.667 0.05 0.1 0 0 LorenzL ;
: x 7 13 10 2.1 0.1 0.1 0.1 0 LorenzL 3 + 4 1 l ^ 0.5 + * d * l 1 l * 5 / ;
: m d 4 / b * 2 2 1 l ^ l ^ 12 / 10 + b 3 / l 0.5 RoundTo 0.1 Lag2 12 * 35 + 1 11 / l 1 3 / RoundTo 0.1 Lag2 12 / 0.153 + 0.1 0.1 0 0 LorenzL ;
: y 9 b / b 3 / 1 + l 0.5 Max * 2 / m * ;
x y Splay2 13 13 DelayN +
