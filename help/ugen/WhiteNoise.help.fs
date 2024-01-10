\ WhiteNoise ; quiet (left)
WhiteNoise.ar 0.05 *

\ WhiteNoise ; quiet (left & right differ)
[ WhiteNoise WhiteNoise ] 0.05 *

\ WhiteNoise ; quiet (left & right equal)
[ WhiteNoise.ar dup ] 0.05 *

\ WhiteNoise ; quiet (left & right differ) using clone
WhiteNoise.ar 2 clone 0.05 *
