\ tw 0041 (f0) ; http://www.fredrikolofsson.com/f0blog/?q=node/537
: s 0 6 LocalIn 1 Sweep ;
: z [ 1 0.749 6 12 3 4 ] 0 Impulse ;
: o 1 s z RunningMax / 0 SinOsc ;
o Splay2 Tanh 2 / 0.1 * o LocalOut <!
