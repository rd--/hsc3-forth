\ https://swiki.hfbk-hamburg.de/MusicTechnology/899 (nv) L43
: k 10 ;
: o 102 101 2 mce 0 LFSaw.ar ;
: x { m } 1 m 10 / Impulse.ar 0 0 Impulse.ar + ;
: d { m } 1.015 0 DC.ar 1 Sweep ** 64 * 1 % 1 + 200 * m x Latch ;
: f { m } o m x 1 1 m d / 4 0.2 Pluck ;
: p k 0 do i f loop ;
p k mce mix k / 0 0.25 1 0 Line.kr *
