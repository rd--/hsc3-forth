\ F0 - Tw 0298
: c [ 2 3 ] 0 SinOscFb 1 % ;
: f 1 0 SinOscFb Ceil 36 * 2 0 SinOscFb 1 RoundTo 2 * 4 + 12 * 1 [ 4 12 ] / 0 SinOscFb 1 XFade2 8 + 4 * ;
f c SinOscFb c * 1 1 3 / SinOscFb *
