\ One-line (Ljp)
[ 1 0.99 ] [ 0 0.6 ] LfSaw.ar 2000 * 2000 + ( lfs )
[ 400 600 ] Trunc [ 1 -1 ] * ( lfs_t )
mix 0.98 OnePole ( f )
0 SinOsc.ar 0 0.05 Pan2
