\ https://sccode.org/1-4Qy ; f0 ; 0250
: f 1.50055 [ 0 0.0005 ] 2 ! LfSaw [ 15 4 2 ] 0 LfSaw Latch [ 999 400 150 ] * ;
: o f 0 LfSaw Mix ;
o 4 / 3 2 0.1 [ 0 1 ] LfSaw - 1 AllpassC
