\ 2012-04-19 (f0) ; https://www.listarc.bham.ac.uk/lists/sc-users/msg17536.html
: sw 10 0 LfSaw 0.011 * ;
: t0 [ 5 6 ] 0.1 * 0 LfTri ;
: t1 0.1 0 LfTri 0.05 * 0.05 + ;
sw t0 1 Brf 1 0.01 Limiter 0.1 t1 0.01 RoundTo 1 CombN 0.1 *
