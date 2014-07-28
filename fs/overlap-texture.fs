: texture-env { sus trans }
  0 3 no-reset-node no-loop-node 1 trans env-sin 0 1 sus env-sin 0 0 trans env-sin 0 16 mce ;

: with-texture-env { sus trans } 1 1 0 1 remove-synth sus trans texture-env EnvGen.kr * ;

: spawn-texture { ugen iot rep } rep 0 do ugen play iot pause loop ;

: xfade-texture { ugen sus trans rep }
  rep 0 do ugen sus trans with-texture-env play sus trans + pause loop ;

: overlaps-iot { sus trans overs } trans 2 * sus + overs / ;

: overlap-texture { ugen sus trans overs rep }
  rep 0 do ugen sus trans with-texture-env play sus trans overs overlaps-iot pause loop ;
