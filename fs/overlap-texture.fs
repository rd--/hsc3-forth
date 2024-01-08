: textureEnv { sus trans }
  [ 0 3 noResetNode noLoopNode 1 trans envSin 0 1 sus envSin 0 0 trans envSin 0 ] ;

: withTextureEnv { sus trans } 1 1 0 1 removeSynth sus trans textureEnv EnvGen.kr * ;

: spawnTexture { ugen iot rep } rep 0 do ugen play iot pause loop ;

: xfadeTexture { ugen sus trans rep }
  rep 0 do ugen sus trans withTextureEnv play sus trans + pause loop ;

: overlapsIot { sus trans overs } trans 2 * sus + overs / ;

: overlapTexture { ugen sus trans overs rep }
  rep 0 do ugen sus trans withTextureEnv play sus trans overs overlapsIot pause loop ;

: texturePostProc { f } 0 2 In.ar f execute 0 swap Out -1 addToTail 1 playAt ;
