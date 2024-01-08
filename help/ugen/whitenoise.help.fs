\ quiet whitenoise (left)
whitenoise.ar 0.05 *

\ quiet whitenoise (left & right equal)
[ whitenoise.ar dup ] 0.05 *

\ quiet whitenoise (left & right differ)
whitenoise.ar 2 clone 0.05 *
