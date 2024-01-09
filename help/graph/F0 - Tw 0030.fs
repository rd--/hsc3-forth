\ tw 0030 (f0) ; http://www.fredrikolofsson.com/f0blog/?q=node/537
: f 1 0 LfPar 5 * [ 0.05 0.04 ] 0 LfPar 50 * 160 + 50 RoundTo + ;
: w 0.2 0 LfPar 0.5 * 3 0 LfPar 0.2 * 0.5 + + ;
: o f 0 w VarSaw 8 / ;
o 80 3 0.5 0.5 15 1 0.7 0.5 300 GVerb transpose Mix 0.1 *
