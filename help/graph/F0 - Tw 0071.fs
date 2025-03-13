\ https://fredrikolofsson.com/f0blog/sapf-sctweets/ 0071
: a lftri ;
: b [ 2 3 4 5 ] ;
: c b 9 / 9 / 9 / 0 a ;
: d 9 b - 99 * 9 b - 9 / a b 9 / b 99 / a * ;
: e c d roundto ;
: f 9 0 a 9 b - 99 * * 99 b * + ;
e f * abs 0 a splay2 5 /
