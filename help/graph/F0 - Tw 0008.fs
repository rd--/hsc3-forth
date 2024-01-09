\ https://sccode.org/1-4Qy ; tweet0008
: x [ 0.5 0.5 ] LfNoise1 ;
x -1 1 1.8 1.98 LinLin Crackle
200 2000 x TExpRand 2 Lag
x -1 1 0.0005 0.001 LinLin
0.0012 Formlet 0.25 *
