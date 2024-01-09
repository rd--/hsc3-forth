\ tw 0051 (f0) ; http://sccode.org/1-4Qy
: z [ 115 117 110 100 97 121 ] Saw ;
: f 9 [ 115 108 111 119 ] / Saw 400 * 500 + ;
: w 7 [ 99 111 100 105 110 103 ] / Saw 1.1 + ;
: l z f w BBandPass 9 / ;
l Splay2 10 3 0.5 0.5 15 1 0.7 0.5 300 GVerb transpose Mix
