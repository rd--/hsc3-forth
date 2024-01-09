\ https://sccode.org/1-4Qy ; f0 ; 0248
: d 0.1 0 LfTri 0 < ;
: b d 2 8 upTo + d 2 + / ;
: o b 99 * 0 LfTri ;
: r b 2 / 0 LfTri 0 > b 99 * 5 0.01 0 LfTri 2 + b - 0 LfTri ^ 4 / Ringz ;
o r + Splay2 Tanh 4 /
