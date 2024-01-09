\ https://sccode.org/1-4Qy ; f0 ; 0278
: b 1 2 / 2 6 to ^ ;
: o b 0 LfSaw 0 > 3 * 15 + b / 0 LfSaw ;
: f pi b 0 LfSaw 1 + b * 0 LfSaw ^ 999 * b 9 * 0 LfSaw 9 * 0 LfSaw 99 * + 2 / ;
o f 0.2 Rlpf Splay2 5 /
