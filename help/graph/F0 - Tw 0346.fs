\ F0 - Tw 0346
: c 1 1 [ 3 2 ] / 0 SinOscFb 0.5 RoundTo - 0 2 InFeedback + 0 SinOscFb ;
2 1 c - 0 SinOscFb 1 c - RoundTo ^ 400 * c Abs SinOscFb c 0.1 Lag * c * Tanh
