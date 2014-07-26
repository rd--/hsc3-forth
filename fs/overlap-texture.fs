: env-asr { atk sus rel }
  0 3 no-reset-node no-loop-node 1 atk env-sin 0 1 sus env-lin 0 0 rel env-sin 0 16 mce ;

: with-env { sus trans } 1 1 0 1 remove-synth trans sus trans env-asr EnvGen.kr * ;

: spawn { ugen iot rep } rep 0 do ugen play iot pause loop ;

: xfade-texture { ugen sus trans rep }
  rep 0 do ugen sus trans with-env play sus trans + pause loop ;

: overlaps-iot { sus trans overs } trans 2 * sus + overs / ;

: overlap-texture { ugen sus trans overs rep }
  rep 0 do ugen sus trans with-env play sus trans overs overlaps-iot pause loop ;
