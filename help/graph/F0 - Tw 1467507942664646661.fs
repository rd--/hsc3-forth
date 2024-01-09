\ F0 - Tw 1467507942664646661
: b [ 3 4 5 6 7 8 1 2 ] ;
: fb1 { freq } freq 1 0.1 1.1 0.5 0.1 0.1 FbSineC ;
: d b 12 / fb1 0 > 80 / 1.051 + ;
: c 1 b / fb1 1 + ;
: p 999 b * 2 b 9 / fb1 ^ 4 * 1 - b 8 / fb1 2 / 1 + 99 1 4 / fb1 ^ 99 / + d 1 b / fb1 40 / 0.1 + 0.1 0.1 FbSineC ;
: q 27 b + fb1 0 > 2 + b + d * 1400 * 2 c c 2 / 2 2 0.1 FbSineC ;
3 1 b / fb1 ^ 5 / p * q 12 / + 32 fb1 0 < 3 Hpf b 70 * d / fb1 * 1 2 / fb1 * + Splay2 2 /
