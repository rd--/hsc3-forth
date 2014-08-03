(define LocalBuf
  (lambda (rt numChannels numFrames)
    (mk-ugen (list "LocalBuf" rt (list numChannels numFrames) #f 1 #f (incr-uid)))))
(define SetBuf
  (lambda (rt buf numValues offset values)
    (mk-ugen (list "SetBuf" rt (list buf numValues offset) values 1 #f #f))))
(define A2K
  (lambda (rt in)
    (mk-ugen (list "A2K" rt (list in) #f 1 #f #f))))
(define APF
  (lambda (rt in freq radius)
    (mk-ugen (list "APF" rt (list in freq radius) #f 1 #f #f))))
(define AllpassC
  (lambda (in maxdelaytime delaytime decaytime)
    (mk-ugen (list "AllpassC" #f (list in maxdelaytime delaytime decaytime) #f 1 #f #f))))
(define AllpassL
  (lambda (in maxdelaytime delaytime decaytime)
    (mk-ugen (list "AllpassL" #f (list in maxdelaytime delaytime decaytime) #f 1 #f #f))))
(define AllpassN
  (lambda (in maxdelaytime delaytime decaytime)
    (mk-ugen (list "AllpassN" #f (list in maxdelaytime delaytime decaytime) #f 1 #f #f))))
(define AmpComp
  (lambda (rt freq root exp)
    (mk-ugen (list "AmpComp" rt (list freq root exp) #f 1 #f #f))))
(define AmpCompA
  (lambda (rt freq root minAmp rootAmp)
    (mk-ugen (list "AmpCompA" rt (list freq root minAmp rootAmp) #f 1 #f #f))))
(define Amplitude
  (lambda (rt in attackTime releaseTime)
    (mk-ugen (list "Amplitude" rt (list in attackTime releaseTime) #f 1 #f #f))))
(define AudioControl
  (lambda (rt values)
    (mk-ugen (list "AudioControl" rt (list values) #f 1 #f #f))))
(define BAllPass
  (lambda (in freq rq)
    (mk-ugen (list "BAllPass" #f (list in freq rq) #f 1 #f #f))))
(define BBandPass
  (lambda (in freq bw)
    (mk-ugen (list "BBandPass" #f (list in freq bw) #f 1 #f #f))))
(define BBandStop
  (lambda (in freq bw)
    (mk-ugen (list "BBandStop" #f (list in freq bw) #f 1 #f #f))))
(define BHiPass
  (lambda (in freq rq)
    (mk-ugen (list "BHiPass" #f (list in freq rq) #f 1 #f #f))))
(define BHiShelf
  (lambda (in freq rs db)
    (mk-ugen (list "BHiShelf" #f (list in freq rs db) #f 1 #f #f))))
(define BLowPass
  (lambda (in freq rq)
    (mk-ugen (list "BLowPass" #f (list in freq rq) #f 1 #f #f))))
(define BLowShelf
  (lambda (in freq rs db)
    (mk-ugen (list "BLowShelf" #f (list in freq rs db) #f 1 #f #f))))
(define BPF
  (lambda (in freq rq)
    (mk-ugen (list "BPF" #f (list in freq rq) #f 1 #f #f))))
(define BPZ2
  (lambda (in)
    (mk-ugen (list "BPZ2" #f (list in) #f 1 #f #f))))
(define BPeakEQ
  (lambda (in freq rq db)
    (mk-ugen (list "BPeakEQ" #f (list in freq rq db) #f 1 #f #f))))
(define BRF
  (lambda (in freq rq)
    (mk-ugen (list "BRF" #f (list in freq rq) #f 1 #f #f))))
(define BRZ2
  (lambda (in)
    (mk-ugen (list "BRZ2" #f (list in) #f 1 #f #f))))
(define Balance2
  (lambda (rt left right pos level)
    (mk-ugen (list "Balance2" rt (list left right pos level) #f 2 #f #f))))
(define Ball
  (lambda (rt in g damp friction)
    (mk-ugen (list "Ball" rt (list in g damp friction) #f 1 #f #f))))
(define BeatTrack
  (lambda (rt chain lock)
    (mk-ugen (list "BeatTrack" rt (list chain lock) #f 1 #f #f))))
(define BeatTrack2
  (lambda (rt busindex numfeatures windowsize phaseaccuracy lock weightingscheme)
    (mk-ugen (list "BeatTrack2" rt (list busindex numfeatures windowsize phaseaccuracy lock weightingscheme) #f 6 #f #f))))
(define BiPanB2
  (lambda (rt inA inB azimuth gain)
    (mk-ugen (list "BiPanB2" rt (list inA inB azimuth gain) #f 3 #f #f))))
(define BinaryOpUGen
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 #f #f))))
(define Blip
  (lambda (rt freq numharm)
    (mk-ugen (list "Blip" rt (list freq numharm) #f 1 #f #f))))
(define BlockSize
  (lambda (rt)
    (mk-ugen (list "BlockSize" rt (list ) #f 1 #f #f))))
(define BrownNoise
  (lambda (rt)
    (mk-ugen (list "BrownNoise" rt (list ) #f 1 #f (incr-uid)))))
(define BufAllpassC
  (lambda (rt buf in delaytime decaytime)
    (mk-ugen (list "BufAllpassC" rt (list buf in delaytime decaytime) #f 1 #f #f))))
(define BufAllpassL
  (lambda (rt buf in delaytime decaytime)
    (mk-ugen (list "BufAllpassL" rt (list buf in delaytime decaytime) #f 1 #f #f))))
(define BufAllpassN
  (lambda (rt buf in delaytime decaytime)
    (mk-ugen (list "BufAllpassN" rt (list buf in delaytime decaytime) #f 1 #f #f))))
(define BufChannels
  (lambda (rt bufnum)
    (mk-ugen (list "BufChannels" rt (list bufnum) #f 1 #f #f))))
(define BufCombC
  (lambda (rt buf in delaytime decaytime)
    (mk-ugen (list "BufCombC" rt (list buf in delaytime decaytime) #f 1 #f #f))))
(define BufCombL
  (lambda (rt buf in delaytime decaytime)
    (mk-ugen (list "BufCombL" rt (list buf in delaytime decaytime) #f 1 #f #f))))
(define BufCombN
  (lambda (rt buf in delaytime decaytime)
    (mk-ugen (list "BufCombN" rt (list buf in delaytime decaytime) #f 1 #f #f))))
(define BufDelayC
  (lambda (rt buf in delaytime)
    (mk-ugen (list "BufDelayC" rt (list buf in delaytime) #f 1 #f #f))))
(define BufDelayL
  (lambda (rt buf in delaytime)
    (mk-ugen (list "BufDelayL" rt (list buf in delaytime) #f 1 #f #f))))
(define BufDelayN
  (lambda (rt buf in delaytime)
    (mk-ugen (list "BufDelayN" rt (list buf in delaytime) #f 1 #f #f))))
(define BufDur
  (lambda (rt bufnum)
    (mk-ugen (list "BufDur" rt (list bufnum) #f 1 #f #f))))
(define BufFrames
  (lambda (rt bufnum)
    (mk-ugen (list "BufFrames" rt (list bufnum) #f 1 #f #f))))
(define BufInfoUGenBase
  (lambda (rt bufnum)
    (mk-ugen (list "BufInfoUGenBase" rt (list bufnum) #f 1 #f #f))))
(define BufRateScale
  (lambda (rt bufnum)
    (mk-ugen (list "BufRateScale" rt (list bufnum) #f 1 #f #f))))
(define BufRd
  (lambda (nc rt bufnum phase loop interpolation)
    (mk-ugen (list "BufRd" rt (list bufnum phase loop interpolation) #f nc #f #f))))
(define BufSampleRate
  (lambda (rt bufnum)
    (mk-ugen (list "BufSampleRate" rt (list bufnum) #f 1 #f #f))))
(define BufSamples
  (lambda (rt bufnum)
    (mk-ugen (list "BufSamples" rt (list bufnum) #f 1 #f #f))))
(define BufWr
  (lambda (rt inputArray bufnum phase loop)
    (mk-ugen (list "BufWr" rt (list inputArray bufnum phase) loop 1 #f #f))))
(define COsc
  (lambda (rt bufnum freq beats)
    (mk-ugen (list "COsc" rt (list bufnum freq beats) #f 1 #f #f))))
(define Changed
  (lambda (rt input threshold)
    (mk-ugen (list "Changed" rt (list input threshold) #f 1 #f #f))))
(define CheckBadValues
  (lambda (rt in id post)
    (mk-ugen (list "CheckBadValues" rt (list in id post) #f 1 #f #f))))
(define Clip
  (lambda (in lo hi)
    (mk-ugen (list "Clip" #f (list in lo hi) #f 1 #f #f))))
(define ClipNoise
  (lambda (rt)
    (mk-ugen (list "ClipNoise" rt (list ) #f 1 #f (incr-uid)))))
(define CoinGate
  (lambda (rt prob in)
    (mk-ugen (list "CoinGate" rt (list prob in) #f 1 #f (incr-uid)))))
(define CombC
  (lambda (in maxdelaytime delaytime decaytime)
    (mk-ugen (list "CombC" #f (list in maxdelaytime delaytime decaytime) #f 1 #f #f))))
(define CombL
  (lambda (in maxdelaytime delaytime decaytime)
    (mk-ugen (list "CombL" #f (list in maxdelaytime delaytime decaytime) #f 1 #f #f))))
(define CombN
  (lambda (in maxdelaytime delaytime decaytime)
    (mk-ugen (list "CombN" #f (list in maxdelaytime delaytime decaytime) #f 1 #f #f))))
(define Compander
  (lambda (in control thresh slopeBelow slopeAbove clampTime relaxTime)
    (mk-ugen (list "Compander" #f (list in control thresh slopeBelow slopeAbove clampTime relaxTime) #f 1 #f #f))))
(define CompanderD
  (lambda (rt in thresh slopeBelow slopeAbove clampTime relaxTime)
    (mk-ugen (list "CompanderD" rt (list in thresh slopeBelow slopeAbove clampTime relaxTime) #f 1 #f #f))))
(define Control
  (lambda (rt values)
    (mk-ugen (list "Control" rt (list values) #f 1 #f #f))))
(define ControlDur
  (lambda (rt)
    (mk-ugen (list "ControlDur" rt (list ) #f 1 #f #f))))
(define ControlRate
  (lambda (rt)
    (mk-ugen (list "ControlRate" rt (list ) #f 1 #f #f))))
(define Convolution
  (lambda (rt in kernel framesize)
    (mk-ugen (list "Convolution" rt (list in kernel framesize) #f 1 #f #f))))
(define Convolution2
  (lambda (rt in kernel trigger framesize)
    (mk-ugen (list "Convolution2" rt (list in kernel trigger framesize) #f 1 #f #f))))
(define Convolution2L
  (lambda (rt in kernel trigger framesize crossfade)
    (mk-ugen (list "Convolution2L" rt (list in kernel trigger framesize crossfade) #f 1 #f #f))))
(define Convolution3
  (lambda (rt in kernel trigger framesize)
    (mk-ugen (list "Convolution3" rt (list in kernel trigger framesize) #f 1 #f #f))))
(define Crackle
  (lambda (rt chaosParam)
    (mk-ugen (list "Crackle" rt (list chaosParam) #f 1 #f #f))))
(define CuspL
  (lambda (rt freq a b xi)
    (mk-ugen (list "CuspL" rt (list freq a b xi) #f 1 #f #f))))
(define CuspN
  (lambda (rt freq a b xi)
    (mk-ugen (list "CuspN" rt (list freq a b xi) #f 1 #f #f))))
(define DC
  (lambda (rt in)
    (mk-ugen (list "DC" rt (list in) #f 1 #f #f))))
(define Dbrown
  (lambda (rt lo hi step length)
    (mk-ugen (list "Dbrown" rt (list lo hi step length) #f 1 #f #f))))
(define Dbufrd
  (lambda (rt bufnum phase loop)
    (mk-ugen (list "Dbufrd" rt (list bufnum phase loop) #f 1 #f #f))))
(define Dbufwr
  (lambda (rt input bufnum phase loop)
    (mk-ugen (list "Dbufwr" rt (list input bufnum phase loop) #f 1 #f #f))))
(define Decay
  (lambda (in decayTime)
    (mk-ugen (list "Decay" #f (list in decayTime) #f 1 #f #f))))
(define Decay2
  (lambda (in attackTime decayTime)
    (mk-ugen (list "Decay2" #f (list in attackTime decayTime) #f 1 #f #f))))
(define DecodeB2
  (lambda (nc rt w x y orientation)
    (mk-ugen (list "DecodeB2" rt (list w x y orientation) #f nc #f #f))))
(define DegreeToKey
  (lambda (bufnum in octave)
    (mk-ugen (list "DegreeToKey" #f (list bufnum in octave) #f 1 #f #f))))
(define DelTapRd
  (lambda (rt buffer phase delTime interp)
    (mk-ugen (list "DelTapRd" rt (list buffer phase delTime interp) #f 1 #f #f))))
(define DelTapWr
  (lambda (rt buffer in)
    (mk-ugen (list "DelTapWr" rt (list buffer in) #f 1 #f #f))))
(define Delay1
  (lambda (in)
    (mk-ugen (list "Delay1" #f (list in) #f 1 #f #f))))
(define Delay2
  (lambda (in)
    (mk-ugen (list "Delay2" #f (list in) #f 1 #f #f))))
(define DelayC
  (lambda (in maxdelaytime delaytime)
    (mk-ugen (list "DelayC" #f (list in maxdelaytime delaytime) #f 1 #f #f))))
(define DelayL
  (lambda (in maxdelaytime delaytime)
    (mk-ugen (list "DelayL" #f (list in maxdelaytime delaytime) #f 1 #f #f))))
(define DelayN
  (lambda (in maxdelaytime delaytime)
    (mk-ugen (list "DelayN" #f (list in maxdelaytime delaytime) #f 1 #f #f))))
(define Demand
  (lambda (rt trig reset demandUGens)
    (mk-ugen (list "Demand" rt (list trig reset demandUGens) #f 1 #f #f))))
(define DemandEnvGen
  (lambda (rt level dur shape curve gate reset levelScale levelBias timeScale doneAction)
    (mk-ugen (list "DemandEnvGen" rt (list level dur shape curve gate reset levelScale levelBias timeScale doneAction) #f 1 #f #f))))
(define DetectIndex
  (lambda (rt bufnum in)
    (mk-ugen (list "DetectIndex" rt (list bufnum in) #f 1 #f #f))))
(define DetectSilence
  (lambda (rt in amp time doneAction)
    (mk-ugen (list "DetectSilence" rt (list in amp time doneAction) #f 1 #f #f))))
(define Dgeom
  (lambda (rt start grow length)
    (mk-ugen (list "Dgeom" rt (list start grow length) #f 1 #f #f))))
(define Dibrown
  (lambda (rt lo hi step length)
    (mk-ugen (list "Dibrown" rt (list lo hi step length) #f 1 #f #f))))
(define DiskIn
  (lambda (nc rt bufnum loop)
    (mk-ugen (list "DiskIn" rt (list bufnum loop) #f nc #f #f))))
(define DiskOut
  (lambda (rt bufnum channelsArray)
    (mk-ugen (list "DiskOut" rt (list bufnum) channelsArray 1 #f #f))))
(define Diwhite
  (lambda (rt lo hi length)
    (mk-ugen (list "Diwhite" rt (list lo hi length) #f 1 #f #f))))
(define Donce
  (lambda (rt in)
    (mk-ugen (list "Donce" rt (list in) #f 1 #f #f))))
(define Done
  (lambda (rt src)
    (mk-ugen (list "Done" rt (list src) #f 1 #f #f))))
(define Dpoll
  (lambda (rt in label run trigid)
    (mk-ugen (list "Dpoll" rt (list in label run trigid) #f 1 #f #f))))
(define Drand
  (lambda (rt list repeats)
    (mk-ugen (list "Drand" rt (list list) repeats 1 #f (incr-uid)))))
(define Dreset
  (lambda (rt in reset)
    (mk-ugen (list "Dreset" rt (list in reset) #f 1 #f #f))))
(define Dseq
  (lambda (rt list repeats)
    (mk-ugen (list "Dseq" rt (list list) repeats 1 #f (incr-uid)))))
(define Dser
  (lambda (rt list repeats)
    (mk-ugen (list "Dser" rt (list list) repeats 1 #f #f))))
(define Dseries
  (lambda (rt start step length)
    (mk-ugen (list "Dseries" rt (list start step length) #f 1 #f #f))))
(define Dshuf
  (lambda (rt list repeats)
    (mk-ugen (list "Dshuf" rt (list list repeats) #f 1 #f (incr-uid)))))
(define Dstutter
  (lambda (rt n in)
    (mk-ugen (list "Dstutter" rt (list n in) #f 1 #f #f))))
(define Dswitch
  (lambda (rt list index)
    (mk-ugen (list "Dswitch" rt (list list) index 1 #f #f))))
(define Dswitch1
  (lambda (rt list index)
    (mk-ugen (list "Dswitch1" rt (list list) index 1 #f #f))))
(define Dunique
  (lambda (rt source maxBufferSize protected)
    (mk-ugen (list "Dunique" rt (list source maxBufferSize protected) #f 1 #f #f))))
(define Dust
  (lambda (rt density)
    (mk-ugen (list "Dust" rt (list density) #f 1 #f (incr-uid)))))
(define Dust2
  (lambda (rt density)
    (mk-ugen (list "Dust2" rt (list density) #f 1 #f (incr-uid)))))
(define DustR
  (lambda (rt iot_min iot_max)
    (mk-ugen (list "DustR" rt (list iot_min iot_max) #f 1 #f #f))))
(define Duty
  (lambda (rt dur reset level doneAction)
    (mk-ugen (list "Duty" rt (list dur reset level doneAction) #f 1 #f #f))))
(define Dwhite
  (lambda (rt lo hi length)
    (mk-ugen (list "Dwhite" rt (list lo hi length) #f 1 #f #f))))
(define Dwrand
  (lambda (rt list weights repeats)
    (mk-ugen (list "Dwrand" rt (list list weights repeats) #f 1 #f (incr-uid)))))
(define Dxrand
  (lambda (rt list repeats)
    (mk-ugen (list "Dxrand" rt (list list) repeats 1 #f #f))))
(define DynKlang
  (lambda (rt specificationsArrayRef freqscale freqoffset)
    (mk-ugen (list "DynKlang" rt (list specificationsArrayRef freqscale freqoffset) #f 1 #f #f))))
(define DynKlank
  (lambda (rt specificationsArrayRef input freqscale freqoffset decayscale)
    (mk-ugen (list "DynKlank" rt (list specificationsArrayRef input freqscale freqoffset decayscale) #f 1 #f #f))))
(define EnvGen
  (lambda (rt envelope gate levelScale levelBias timeScale doneAction)
    (mk-ugen (list "EnvGen" rt (list envelope gate levelScale levelBias timeScale) doneAction 1 #f #f))))
(define ExpRand
  (lambda (rt lo hi)
    (mk-ugen (list "ExpRand" rt (list lo hi) #f 1 #f (incr-uid)))))
(define FBSineC
  (lambda (rt freq im fb a c xi yi)
    (mk-ugen (list "FBSineC" rt (list freq im fb a c xi yi) #f 1 #f #f))))
(define FBSineL
  (lambda (rt freq im fb a c xi yi)
    (mk-ugen (list "FBSineL" rt (list freq im fb a c xi yi) #f 1 #f #f))))
(define FBSineN
  (lambda (rt freq im fb a c xi yi)
    (mk-ugen (list "FBSineN" rt (list freq im fb a c xi yi) #f 1 #f #f))))
(define FFT
  (lambda (rt buffer in hop wintype active winsize)
    (mk-ugen (list "FFT" rt (list buffer in hop wintype active winsize) #f 1 #f #f))))
(define FOS
  (lambda (in a0 a1 b1)
    (mk-ugen (list "FOS" #f (list in a0 a1 b1) #f 1 #f #f))))
(define FSinOsc
  (lambda (rt freq iphase)
    (mk-ugen (list "FSinOsc" rt (list freq iphase) #f 1 #f #f))))
(define Fold
  (lambda (in lo hi)
    (mk-ugen (list "Fold" #f (list in lo hi) #f 1 #f #f))))
(define Formant
  (lambda (rt fundfreq formfreq bwfreq)
    (mk-ugen (list "Formant" rt (list fundfreq formfreq bwfreq) #f 1 #f #f))))
(define Formlet
  (lambda (in freq attacktime decaytime)
    (mk-ugen (list "Formlet" #f (list in freq attacktime decaytime) #f 1 #f #f))))
(define Free
  (lambda (rt trig id)
    (mk-ugen (list "Free" rt (list trig id) #f 1 #f #f))))
(define FreeSelf
  (lambda (rt in)
    (mk-ugen (list "FreeSelf" rt (list in) #f 1 #f #f))))
(define FreeSelfWhenDone
  (lambda (rt src)
    (mk-ugen (list "FreeSelfWhenDone" rt (list src) #f 1 #f #f))))
(define FreeVerb
  (lambda (in mix room damp)
    (mk-ugen (list "FreeVerb" #f (list in mix room damp) #f 1 #f #f))))
(define FreeVerb2
  (lambda (in in2 mix room damp)
    (mk-ugen (list "FreeVerb2" #f (list in in2 mix room damp) #f 2 #f #f))))
(define FreqShift
  (lambda (rt in freq phase)
    (mk-ugen (list "FreqShift" rt (list in freq phase) #f 1 #f #f))))
(define GVerb
  (lambda (in roomsize revtime damping inputbw spread drylevel earlyreflevel taillevel maxroomsize)
    (mk-ugen (list "GVerb" #f (list in roomsize revtime damping inputbw spread drylevel earlyreflevel taillevel maxroomsize) #f 2 #f #f))))
(define Gate
  (lambda (in trig)
    (mk-ugen (list "Gate" #f (list in trig) #f 1 #f #f))))
(define GbmanL
  (lambda (rt freq xi yi)
    (mk-ugen (list "GbmanL" rt (list freq xi yi) #f 1 #f #f))))
(define GbmanN
  (lambda (rt freq xi yi)
    (mk-ugen (list "GbmanN" rt (list freq xi yi) #f 1 #f #f))))
(define Gendy1
  (lambda (rt ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum)
    (mk-ugen (list "Gendy1" rt (list ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum) #f 1 #f (incr-uid)))))
(define Gendy2
  (lambda (rt ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum a c)
    (mk-ugen (list "Gendy2" rt (list ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum a c) #f 1 #f (incr-uid)))))
(define Gendy3
  (lambda (rt ampdist durdist adparam ddparam freq ampscale durscale initCPs knum)
    (mk-ugen (list "Gendy3" rt (list ampdist durdist adparam ddparam freq ampscale durscale initCPs knum) #f 1 #f (incr-uid)))))
(define GrainBuf
  (lambda (nc rt trigger dur sndbuf rate pos interp pan envbufnum maxGrains)
    (mk-ugen (list "GrainBuf" rt (list trigger dur sndbuf rate pos interp pan envbufnum maxGrains) #f nc #f #f))))
(define GrainFM
  (lambda (nc rt trigger dur carfreq modfreq index pan envbufnum maxGrains)
    (mk-ugen (list "GrainFM" rt (list trigger dur carfreq modfreq index pan envbufnum maxGrains) #f nc #f #f))))
(define GrainIn
  (lambda (nc rt trigger dur in pan envbufnum maxGrains)
    (mk-ugen (list "GrainIn" rt (list trigger dur in pan envbufnum maxGrains) #f nc #f #f))))
(define GrainSin
  (lambda (nc rt trigger dur freq pan envbufnum maxGrains)
    (mk-ugen (list "GrainSin" rt (list trigger dur freq pan envbufnum maxGrains) #f nc #f #f))))
(define GrayNoise
  (lambda (rt)
    (mk-ugen (list "GrayNoise" rt (list ) #f 1 #f (incr-uid)))))
(define HPF
  (lambda (in freq)
    (mk-ugen (list "HPF" #f (list in freq) #f 1 #f #f))))
(define HPZ1
  (lambda (in)
    (mk-ugen (list "HPZ1" #f (list in) #f 1 #f #f))))
(define HPZ2
  (lambda (in)
    (mk-ugen (list "HPZ2" #f (list in) #f 1 #f #f))))
(define Hasher
  (lambda (in)
    (mk-ugen (list "Hasher" #f (list in) #f 1 #f #f))))
(define HenonC
  (lambda (rt freq a b x0 x1)
    (mk-ugen (list "HenonC" rt (list freq a b x0 x1) #f 1 #f #f))))
(define HenonL
  (lambda (rt freq a b x0 x1)
    (mk-ugen (list "HenonL" rt (list freq a b x0 x1) #f 1 #f #f))))
(define HenonN
  (lambda (rt freq a b x0 x1)
    (mk-ugen (list "HenonN" rt (list freq a b x0 x1) #f 1 #f #f))))
(define Hilbert
  (lambda (in)
    (mk-ugen (list "Hilbert" #f (list in) #f 2 #f #f))))
(define HilbertFIR
  (lambda (rt in buffer)
    (mk-ugen (list "HilbertFIR" rt (list in buffer) #f 2 #f #f))))
(define IEnvGen
  (lambda (rt envelope index)
    (mk-ugen (list "IEnvGen" rt (list envelope index) #f 1 #f #f))))
(define IFFT
  (lambda (rt buffer wintype winsize)
    (mk-ugen (list "IFFT" rt (list buffer wintype winsize) #f 1 #f #f))))
(define IRand
  (lambda (rt lo hi)
    (mk-ugen (list "IRand" rt (list lo hi) #f 1 #f (incr-uid)))))
(define Impulse
  (lambda (rt freq phase)
    (mk-ugen (list "Impulse" rt (list freq phase) #f 1 #f #f))))
(define In
  (lambda (nc rt bus)
    (mk-ugen (list "In" rt (list bus) #f nc #f #f))))
(define InBus
  (lambda (rt bus offset clip)
    (mk-ugen (list "InBus" rt (list bus offset clip) #f 1 #f #f))))
(define InFeedback
  (lambda (nc rt bus)
    (mk-ugen (list "InFeedback" rt (list bus) #f nc #f #f))))
(define InRange
  (lambda (in lo hi)
    (mk-ugen (list "InRange" #f (list in lo hi) #f 1 #f #f))))
(define InRect
  (lambda (rt x y rect)
    (mk-ugen (list "InRect" rt (list x y rect) #f 1 #f #f))))
(define InTrig
  (lambda (nc rt bus)
    (mk-ugen (list "InTrig" rt (list bus) #f nc #f #f))))
(define Index
  (lambda (rt bufnum in)
    (mk-ugen (list "Index" rt (list bufnum in) #f 1 #f #f))))
(define IndexInBetween
  (lambda (rt bufnum in)
    (mk-ugen (list "IndexInBetween" rt (list bufnum in) #f 1 #f #f))))
(define IndexL
  (lambda (rt bufnum in)
    (mk-ugen (list "IndexL" rt (list bufnum in) #f 1 #f #f))))
(define InfoUGenBase
  (lambda (rt)
    (mk-ugen (list "InfoUGenBase" rt (list ) #f 1 #f #f))))
(define Integrator
  (lambda (in coef)
    (mk-ugen (list "Integrator" #f (list in coef) #f 1 #f #f))))
(define K2A
  (lambda (rt in)
    (mk-ugen (list "K2A" rt (list in) #f 1 #f #f))))
(define KeyState
  (lambda (rt keycode minval maxval lag)
    (mk-ugen (list "KeyState" rt (list keycode minval maxval lag) #f 1 #f #f))))
(define KeyTrack
  (lambda (rt chain keydecay chromaleak)
    (mk-ugen (list "KeyTrack" rt (list chain keydecay chromaleak) #f 1 #f #f))))
(define Klang
  (lambda (rt specificationsArrayRef freqscale freqoffset)
    (mk-ugen (list "Klang" rt (list specificationsArrayRef freqscale) freqoffset 1 #f #f))))
(define Klank
  (lambda (specificationsArrayRef input freqscale freqoffset decayscale)
    (mk-ugen (list "Klank" #f (list specificationsArrayRef input freqscale freqoffset) decayscale 1 #f #f))))
(define LFClipNoise
  (lambda (rt freq)
    (mk-ugen (list "LFClipNoise" rt (list freq) #f 1 #f (incr-uid)))))
(define LFCub
  (lambda (rt freq iphase)
    (mk-ugen (list "LFCub" rt (list freq iphase) #f 1 #f #f))))
(define LFDClipNoise
  (lambda (rt freq)
    (mk-ugen (list "LFDClipNoise" rt (list freq) #f 1 #f (incr-uid)))))
(define LFDNoise0
  (lambda (rt freq)
    (mk-ugen (list "LFDNoise0" rt (list freq) #f 1 #f (incr-uid)))))
(define LFDNoise1
  (lambda (rt freq)
    (mk-ugen (list "LFDNoise1" rt (list freq) #f 1 #f (incr-uid)))))
(define LFDNoise3
  (lambda (rt freq)
    (mk-ugen (list "LFDNoise3" rt (list freq) #f 1 #f (incr-uid)))))
(define LFGauss
  (lambda (rt duration width iphase loop doneAction)
    (mk-ugen (list "LFGauss" rt (list duration width iphase loop doneAction) #f 1 #f #f))))
(define LFNoise0
  (lambda (rt freq)
    (mk-ugen (list "LFNoise0" rt (list freq) #f 1 #f (incr-uid)))))
(define LFNoise1
  (lambda (rt freq)
    (mk-ugen (list "LFNoise1" rt (list freq) #f 1 #f (incr-uid)))))
(define LFNoise2
  (lambda (rt freq)
    (mk-ugen (list "LFNoise2" rt (list freq) #f 1 #f (incr-uid)))))
(define LFPar
  (lambda (rt freq iphase)
    (mk-ugen (list "LFPar" rt (list freq iphase) #f 1 #f #f))))
(define LFPulse
  (lambda (rt freq iphase width)
    (mk-ugen (list "LFPulse" rt (list freq iphase width) #f 1 #f #f))))
(define LFSaw
  (lambda (rt freq iphase)
    (mk-ugen (list "LFSaw" rt (list freq iphase) #f 1 #f #f))))
(define LFTri
  (lambda (rt freq iphase)
    (mk-ugen (list "LFTri" rt (list freq iphase) #f 1 #f #f))))
(define LPF
  (lambda (in freq)
    (mk-ugen (list "LPF" #f (list in freq) #f 1 #f #f))))
(define LPZ1
  (lambda (in)
    (mk-ugen (list "LPZ1" #f (list in) #f 1 #f #f))))
(define LPZ2
  (lambda (in)
    (mk-ugen (list "LPZ2" #f (list in) #f 1 #f #f))))
(define Lag
  (lambda (in lagTime)
    (mk-ugen (list "Lag" #f (list in lagTime) #f 1 #f #f))))
(define Lag2
  (lambda (in lagTime)
    (mk-ugen (list "Lag2" #f (list in lagTime) #f 1 #f #f))))
(define Lag2UD
  (lambda (in lagTimeU lagTimeD)
    (mk-ugen (list "Lag2UD" #f (list in lagTimeU lagTimeD) #f 1 #f #f))))
(define Lag3
  (lambda (in lagTime)
    (mk-ugen (list "Lag3" #f (list in lagTime) #f 1 #f #f))))
(define Lag3UD
  (lambda (in lagTimeU lagTimeD)
    (mk-ugen (list "Lag3UD" #f (list in lagTimeU lagTimeD) #f 1 #f #f))))
(define LagControl
  (lambda (rt values lags)
    (mk-ugen (list "LagControl" rt (list values lags) #f 1 #f #f))))
(define LagIn
  (lambda (nc rt bus lag)
    (mk-ugen (list "LagIn" rt (list bus lag) #f nc #f #f))))
(define LagUD
  (lambda (in lagTimeU lagTimeD)
    (mk-ugen (list "LagUD" #f (list in lagTimeU lagTimeD) #f 1 #f #f))))
(define LastValue
  (lambda (in diff)
    (mk-ugen (list "LastValue" #f (list in diff) #f 1 #f #f))))
(define Latch
  (lambda (in trig)
    (mk-ugen (list "Latch" #f (list in trig) #f 1 #f #f))))
(define LatoocarfianC
  (lambda (rt freq a b c d xi yi)
    (mk-ugen (list "LatoocarfianC" rt (list freq a b c d xi yi) #f 1 #f #f))))
(define LatoocarfianL
  (lambda (rt freq a b c d xi yi)
    (mk-ugen (list "LatoocarfianL" rt (list freq a b c d xi yi) #f 1 #f #f))))
(define LatoocarfianN
  (lambda (rt freq a b c d xi yi)
    (mk-ugen (list "LatoocarfianN" rt (list freq a b c d xi yi) #f 1 #f #f))))
(define LeakDC
  (lambda (in coef)
    (mk-ugen (list "LeakDC" #f (list in coef) #f 1 #f #f))))
(define LeastChange
  (lambda (rt a b)
    (mk-ugen (list "LeastChange" rt (list a b) #f 1 #f #f))))
(define Limiter
  (lambda (in level dur)
    (mk-ugen (list "Limiter" #f (list in level dur) #f 1 #f #f))))
(define LinCongC
  (lambda (rt freq a c m xi)
    (mk-ugen (list "LinCongC" rt (list freq a c m xi) #f 1 #f #f))))
(define LinCongL
  (lambda (rt freq a c m xi)
    (mk-ugen (list "LinCongL" rt (list freq a c m xi) #f 1 #f #f))))
(define LinCongN
  (lambda (rt freq a c m xi)
    (mk-ugen (list "LinCongN" rt (list freq a c m xi) #f 1 #f #f))))
(define LinExp
  (lambda (in srclo srchi dstlo dsthi)
    (mk-ugen (list "LinExp" #f (list in srclo srchi dstlo dsthi) #f 1 #f #f))))
(define LinPan2
  (lambda (rt in pos level)
    (mk-ugen (list "LinPan2" rt (list in pos level) #f 2 #f #f))))
(define LinRand
  (lambda (rt lo hi minmax)
    (mk-ugen (list "LinRand" rt (list lo hi minmax) #f 1 #f (incr-uid)))))
(define LinXFade2
  (lambda (rt inA inB pan level)
    (mk-ugen (list "LinXFade2" rt (list inA inB pan level) #f 1 #f #f))))
(define Line
  (lambda (rt start end dur doneAction)
    (mk-ugen (list "Line" rt (list start end dur doneAction) #f 1 #f #f))))
(define Linen
  (lambda (rt gate attackTime susLevel releaseTime doneAction)
    (mk-ugen (list "Linen" rt (list gate attackTime susLevel releaseTime doneAction) #f 1 #f #f))))
(define ListDUGen
  (lambda (rt list repeats)
    (mk-ugen (list "ListDUGen" rt (list list repeats) #f 1 #f #f))))
(define LocalBuf
  (lambda (rt numChannels numFrames)
    (mk-ugen (list "LocalBuf" rt (list numChannels numFrames) #f 1 #f (incr-uid)))))
(define LocalIn
  (lambda (nc rt default)
    (mk-ugen (list "LocalIn" rt (list default) #f nc #f #f))))
(define LocalOut
  (lambda (channelsArray)
    (mk-ugen (list "LocalOut" #f (list ) channelsArray 1 #f #f))))
(define Logistic
  (lambda (rt chaosParam freq init)
    (mk-ugen (list "Logistic" rt (list chaosParam freq init) #f 1 #f #f))))
(define LorenzL
  (lambda (rt freq s r b h xi yi zi)
    (mk-ugen (list "LorenzL" rt (list freq s r b h xi yi zi) #f 1 #f #f))))
(define Loudness
  (lambda (rt chain smask tmask)
    (mk-ugen (list "Loudness" rt (list chain smask tmask) #f 1 #f #f))))
(define MFCC
  (lambda (rt chain numcoeff)
    (mk-ugen (list "MFCC" rt (list chain numcoeff) #f 13 #f #f))))
(define MantissaMask
  (lambda (in bits)
    (mk-ugen (list "MantissaMask" #f (list in bits) #f 1 #f #f))))
(define MaxLocalBufs
  (lambda (rt)
    (mk-ugen (list "MaxLocalBufs" rt (list ) #f 1 #f #f))))
(define Median
  (lambda (length in)
    (mk-ugen (list "Median" #f (list length in) #f 1 #f #f))))
(define MidEQ
  (lambda (in freq rq db)
    (mk-ugen (list "MidEQ" #f (list in freq rq db) #f 1 #f #f))))
(define ModDif
  (lambda (rt x y mod)
    (mk-ugen (list "ModDif" rt (list x y mod) #f 1 #f #f))))
(define MoogFF
  (lambda (in freq gain reset)
    (mk-ugen (list "MoogFF" #f (list in freq gain reset) #f 1 #f #f))))
(define MostChange
  (lambda (a b)
    (mk-ugen (list "MostChange" #f (list a b) #f 1 #f #f))))
(define MouseButton
  (lambda (rt minval maxval lag)
    (mk-ugen (list "MouseButton" rt (list minval maxval lag) #f 1 #f #f))))
(define MouseX
  (lambda (rt minval maxval warp lag)
    (mk-ugen (list "MouseX" rt (list minval maxval warp lag) #f 1 #f #f))))
(define MouseY
  (lambda (rt minval maxval warp lag)
    (mk-ugen (list "MouseY" rt (list minval maxval warp lag) #f 1 #f #f))))
(define NRand
  (lambda (rt lo hi n)
    (mk-ugen (list "NRand" rt (list lo hi n) #f 1 #f (incr-uid)))))
(define Normalizer
  (lambda (in level dur)
    (mk-ugen (list "Normalizer" #f (list in level dur) #f 1 #f #f))))
(define NumAudioBuses
  (lambda (rt)
    (mk-ugen (list "NumAudioBuses" rt (list ) #f 1 #f #f))))
(define NumBuffers
  (lambda (rt)
    (mk-ugen (list "NumBuffers" rt (list ) #f 1 #f #f))))
(define NumControlBuses
  (lambda (rt)
    (mk-ugen (list "NumControlBuses" rt (list ) #f 1 #f #f))))
(define NumInputBuses
  (lambda (rt)
    (mk-ugen (list "NumInputBuses" rt (list ) #f 1 #f #f))))
(define NumOutputBuses
  (lambda (rt)
    (mk-ugen (list "NumOutputBuses" rt (list ) #f 1 #f #f))))
(define NumRunningSynths
  (lambda (rt)
    (mk-ugen (list "NumRunningSynths" rt (list ) #f 1 #f #f))))
(define OffsetOut
  (lambda (rt bus channelsArray)
    (mk-ugen (list "OffsetOut" rt (list bus) channelsArray 1 #f #f))))
(define OnePole
  (lambda (in coef)
    (mk-ugen (list "OnePole" #f (list in coef) #f 1 #f #f))))
(define OneZero
  (lambda (in coef)
    (mk-ugen (list "OneZero" #f (list in coef) #f 1 #f #f))))
(define Onsets
  (lambda (rt chain threshold odftype relaxtime floor mingap medianspan whtype rawodf)
    (mk-ugen (list "Onsets" rt (list chain threshold odftype relaxtime floor mingap medianspan whtype rawodf) #f 1 #f #f))))
(define Osc
  (lambda (rt bufnum freq phase)
    (mk-ugen (list "Osc" rt (list bufnum freq phase) #f 1 #f #f))))
(define OscN
  (lambda (rt bufnum freq phase)
    (mk-ugen (list "OscN" rt (list bufnum freq phase) #f 1 #f #f))))
(define Out
  (lambda (bus channelsArray)
    (mk-ugen (list "Out" #f (list bus) channelsArray 1 #f #f))))
(define PSinGrain
  (lambda (rt freq dur amp)
    (mk-ugen (list "PSinGrain" rt (list freq dur amp) #f 1 #f #f))))
(define PV_Add
  (lambda (rt bufferA bufferB)
    (mk-ugen (list "PV_Add" rt (list bufferA bufferB) #f 1 #f #f))))
(define PV_BinScramble
  (lambda (rt buffer wipe width trig)
    (mk-ugen (list "PV_BinScramble" rt (list buffer wipe width trig) #f 1 #f #f))))
(define PV_BinShift
  (lambda (rt buffer stretch shift interp)
    (mk-ugen (list "PV_BinShift" rt (list buffer stretch shift interp) #f 1 #f #f))))
(define PV_BinWipe
  (lambda (rt bufferA bufferB wipe)
    (mk-ugen (list "PV_BinWipe" rt (list bufferA bufferB wipe) #f 1 #f #f))))
(define PV_BrickWall
  (lambda (rt buffer wipe)
    (mk-ugen (list "PV_BrickWall" rt (list buffer wipe) #f 1 #f #f))))
(define PV_ChainUGen
  (lambda (rt maxSize)
    (mk-ugen (list "PV_ChainUGen" rt (list maxSize) #f 1 #f #f))))
(define PV_ConformalMap
  (lambda (rt buffer areal aimag)
    (mk-ugen (list "PV_ConformalMap" rt (list buffer areal aimag) #f 1 #f #f))))
(define PV_Conj
  (lambda (rt buffer)
    (mk-ugen (list "PV_Conj" rt (list buffer) #f 1 #f #f))))
(define PV_Copy
  (lambda (rt bufferA bufferB)
    (mk-ugen (list "PV_Copy" rt (list bufferA bufferB) #f 1 #f #f))))
(define PV_CopyPhase
  (lambda (rt bufferA bufferB)
    (mk-ugen (list "PV_CopyPhase" rt (list bufferA bufferB) #f 1 #f #f))))
(define PV_Diffuser
  (lambda (rt buffer trig)
    (mk-ugen (list "PV_Diffuser" rt (list buffer trig) #f 1 #f #f))))
(define PV_Div
  (lambda (rt bufferA bufferB)
    (mk-ugen (list "PV_Div" rt (list bufferA bufferB) #f 1 #f #f))))
(define PV_HainsworthFoote
  (lambda (rt maxSize)
    (mk-ugen (list "PV_HainsworthFoote" rt (list maxSize) #f 1 #f #f))))
(define PV_JensenAndersen
  (lambda (rt maxSize)
    (mk-ugen (list "PV_JensenAndersen" rt (list maxSize) #f 1 #f #f))))
(define PV_LocalMax
  (lambda (rt buffer threshold)
    (mk-ugen (list "PV_LocalMax" rt (list buffer threshold) #f 1 #f #f))))
(define PV_MagAbove
  (lambda (rt buffer threshold)
    (mk-ugen (list "PV_MagAbove" rt (list buffer threshold) #f 1 #f #f))))
(define PV_MagBelow
  (lambda (rt buffer threshold)
    (mk-ugen (list "PV_MagBelow" rt (list buffer threshold) #f 1 #f #f))))
(define PV_MagClip
  (lambda (rt buffer threshold)
    (mk-ugen (list "PV_MagClip" rt (list buffer threshold) #f 1 #f #f))))
(define PV_MagDiv
  (lambda (rt bufferA bufferB zeroed)
    (mk-ugen (list "PV_MagDiv" rt (list bufferA bufferB zeroed) #f 1 #f #f))))
(define PV_MagFreeze
  (lambda (rt buffer freeze)
    (mk-ugen (list "PV_MagFreeze" rt (list buffer freeze) #f 1 #f #f))))
(define PV_MagMul
  (lambda (rt bufferA bufferB)
    (mk-ugen (list "PV_MagMul" rt (list bufferA bufferB) #f 1 #f #f))))
(define PV_MagNoise
  (lambda (rt buffer)
    (mk-ugen (list "PV_MagNoise" rt (list buffer) #f 1 #f #f))))
(define PV_MagShift
  (lambda (rt buffer stretch shift)
    (mk-ugen (list "PV_MagShift" rt (list buffer stretch shift) #f 1 #f #f))))
(define PV_MagSmear
  (lambda (rt buffer bins)
    (mk-ugen (list "PV_MagSmear" rt (list buffer bins) #f 1 #f #f))))
(define PV_MagSquared
  (lambda (rt buffer)
    (mk-ugen (list "PV_MagSquared" rt (list buffer) #f 1 #f #f))))
(define PV_Max
  (lambda (rt bufferA bufferB)
    (mk-ugen (list "PV_Max" rt (list bufferA bufferB) #f 1 #f #f))))
(define PV_Min
  (lambda (rt bufferA bufferB)
    (mk-ugen (list "PV_Min" rt (list bufferA bufferB) #f 1 #f #f))))
(define PV_Mul
  (lambda (rt bufferA bufferB)
    (mk-ugen (list "PV_Mul" rt (list bufferA bufferB) #f 1 #f #f))))
(define PV_PhaseShift
  (lambda (rt buffer shift integrate)
    (mk-ugen (list "PV_PhaseShift" rt (list buffer shift integrate) #f 1 #f #f))))
(define PV_PhaseShift270
  (lambda (rt buffer)
    (mk-ugen (list "PV_PhaseShift270" rt (list buffer) #f 1 #f #f))))
(define PV_PhaseShift90
  (lambda (rt buffer)
    (mk-ugen (list "PV_PhaseShift90" rt (list buffer) #f 1 #f #f))))
(define PV_RandComb
  (lambda (rt buffer wipe trig)
    (mk-ugen (list "PV_RandComb" rt (list buffer wipe trig) #f 1 #f #f))))
(define PV_RandWipe
  (lambda (rt bufferA bufferB wipe trig)
    (mk-ugen (list "PV_RandWipe" rt (list bufferA bufferB wipe trig) #f 1 #f #f))))
(define PV_RectComb
  (lambda (rt buffer numTeeth phase width)
    (mk-ugen (list "PV_RectComb" rt (list buffer numTeeth phase width) #f 1 #f #f))))
(define PV_RectComb2
  (lambda (rt bufferA bufferB numTeeth phase width)
    (mk-ugen (list "PV_RectComb2" rt (list bufferA bufferB numTeeth phase width) #f 1 #f #f))))
(define PV_Split
  (lambda (rt bufferA bufferB)
    (mk-ugen (list "PV_Split" rt (list bufferA bufferB) #f 2 #f #f))))
(define Pan2
  (lambda (rt in pos level)
    (mk-ugen (list "Pan2" rt (list in pos level) #f 2 #f #f))))
(define Pan4
  (lambda (rt in xpos ypos level)
    (mk-ugen (list "Pan4" rt (list in xpos ypos level) #f 4 #f #f))))
(define PanAz
  (lambda (nc rt in pos level width orientation)
    (mk-ugen (list "PanAz" rt (list in pos level width orientation) #f nc #f #f))))
(define PanB
  (lambda (rt in azimuth elevation gain)
    (mk-ugen (list "PanB" rt (list in azimuth elevation gain) #f 4 #f #f))))
(define PanB2
  (lambda (rt in azimuth gain)
    (mk-ugen (list "PanB2" rt (list in azimuth gain) #f 3 #f #f))))
(define PartConv
  (lambda (rt in fftsize irbufnum)
    (mk-ugen (list "PartConv" rt (list in fftsize irbufnum) #f 1 #f #f))))
(define Pause
  (lambda (rt gate id)
    (mk-ugen (list "Pause" rt (list gate id) #f 1 #f #f))))
(define PauseSelf
  (lambda (rt in)
    (mk-ugen (list "PauseSelf" rt (list in) #f 1 #f #f))))
(define PauseSelfWhenDone
  (lambda (rt src)
    (mk-ugen (list "PauseSelfWhenDone" rt (list src) #f 1 #f #f))))
(define Peak
  (lambda (in trig)
    (mk-ugen (list "Peak" #f (list in trig) #f 1 #f #f))))
(define PeakFollower
  (lambda (rt in decay)
    (mk-ugen (list "PeakFollower" rt (list in decay) #f 1 #f #f))))
(define Phasor
  (lambda (rt trig rate start end resetPos)
    (mk-ugen (list "Phasor" rt (list trig rate start end resetPos) #f 1 #f #f))))
(define PinkNoise
  (lambda (rt)
    (mk-ugen (list "PinkNoise" rt (list ) #f 1 #f (incr-uid)))))
(define Pitch
  (lambda (rt in initFreq minFreq maxFreq execFreq maxBinsPerOctave median ampThreshold peakThreshold downSample clar)
    (mk-ugen (list "Pitch" rt (list in initFreq minFreq maxFreq execFreq maxBinsPerOctave median ampThreshold peakThreshold downSample clar) #f 2 #f #f))))
(define PitchShift
  (lambda (in windowSize pitchRatio pitchDispersion timeDispersion)
    (mk-ugen (list "PitchShift" #f (list in windowSize pitchRatio pitchDispersion timeDispersion) #f 1 #f #f))))
(define PlayBuf
  (lambda (rt bufnum rate trigger startPos loop doneAction)
    (mk-ugen (list "PlayBuf" rt (list bufnum rate trigger startPos loop doneAction) #f 1 #f #f))))
(define Pluck
  (lambda (in trig maxdelaytime delaytime decaytime coef)
    (mk-ugen (list "Pluck" #f (list in trig maxdelaytime delaytime decaytime coef) #f 1 #f #f))))
(define Poll
  (lambda (rt trig in label trigid)
    (mk-ugen (list "Poll" rt (list trig in label trigid) #f 1 #f #f))))
(define Pulse
  (lambda (rt freq width)
    (mk-ugen (list "Pulse" rt (list freq width) #f 1 #f #f))))
(define PulseCount
  (lambda (trig reset)
    (mk-ugen (list "PulseCount" #f (list trig reset) #f 1 #f #f))))
(define PulseDivider
  (lambda (trig div start)
    (mk-ugen (list "PulseDivider" #f (list trig div start) #f 1 #f #f))))
(define PureMultiOutUGen
  (lambda (rt maxSize)
    (mk-ugen (list "PureMultiOutUGen" rt (list maxSize) #f 1 #f #f))))
(define PureUGen
  (lambda (rt maxSize)
    (mk-ugen (list "PureUGen" rt (list maxSize) #f 1 #f #f))))
(define QuadC
  (lambda (rt freq a b c xi)
    (mk-ugen (list "QuadC" rt (list freq a b c xi) #f 1 #f #f))))
(define QuadL
  (lambda (rt freq a b c xi)
    (mk-ugen (list "QuadL" rt (list freq a b c xi) #f 1 #f #f))))
(define QuadN
  (lambda (rt freq a b c xi)
    (mk-ugen (list "QuadN" rt (list freq a b c xi) #f 1 #f #f))))
(define RDelayMap
  (lambda (rt bufnum in dynamic spec)
    (mk-ugen (list "RDelayMap" rt (list bufnum in dynamic spec) #f 1 #f #f))))
(define RDelaySet
  (lambda (rt in spec)
    (mk-ugen (list "RDelaySet" rt (list in spec) #f 1 #f #f))))
(define RDelaySetB
  (lambda (rt bufnum in spec)
    (mk-ugen (list "RDelaySetB" rt (list bufnum in spec) #f 1 #f #f))))
(define RFreezer
  (lambda (rt bufnum left right gain increment incrementOffset incrementRandom rightRandom syncPhaseTrigger randomizePhaseTrigger numberOfLoops)
    (mk-ugen (list "RFreezer" rt (list bufnum left right gain increment incrementOffset incrementRandom rightRandom syncPhaseTrigger randomizePhaseTrigger numberOfLoops) #f 1 #f #f))))
(define RHPF
  (lambda (in freq rq)
    (mk-ugen (list "RHPF" #f (list in freq rq) #f 1 #f #f))))
(define RLPF
  (lambda (in freq rq)
    (mk-ugen (list "RLPF" #f (list in freq rq) #f 1 #f #f))))
(define RLoopSet
  (lambda (rt bufnum left right gain increment spec)
    (mk-ugen (list "RLoopSet" rt (list bufnum left right gain increment spec) #f 1 #f #f))))
(define RPlayTrace
  (lambda (rt bufnum degree rate axis)
    (mk-ugen (list "RPlayTrace" rt (list bufnum degree rate axis) #f 1 #f #f))))
(define RShufflerB
  (lambda (rt bufnum readLocationMinima readLocationMaxima readIncrementMinima readIncrementMaxima durationMinima durationMaxima envelopeAmplitudeMinima envelopeAmplitudeMaxima envelopeShapeMinima envelopeShapeMaxima envelopeSkewMinima envelopeSkewMaxima stereoLocationMinima stereoLocationMaxima interOffsetTimeMinima interOffsetTimeMaxima ftableReadLocationIncrement readIncrementQuanta interOffsetTimeQuanta)
    (mk-ugen (list "RShufflerB" rt (list bufnum readLocationMinima readLocationMaxima readIncrementMinima readIncrementMaxima durationMinima durationMaxima envelopeAmplitudeMinima envelopeAmplitudeMaxima envelopeShapeMinima envelopeShapeMaxima envelopeSkewMinima envelopeSkewMaxima stereoLocationMinima stereoLocationMaxima interOffsetTimeMinima interOffsetTimeMaxima ftableReadLocationIncrement readIncrementQuanta interOffsetTimeQuanta) #f 2 #f #f))))
(define RShufflerL
  (lambda (rt in fragmentSize maxDelay)
    (mk-ugen (list "RShufflerL" rt (list in fragmentSize maxDelay) #f 1 #f #f))))
(define RTraceRd
  (lambda (rt bufnum degree index axis)
    (mk-ugen (list "RTraceRd" rt (list bufnum degree index axis) #f 1 #f #f))))
(define RTraceRdX
  (lambda (rt bufnum degree index)
    (mk-ugen (list "RTraceRdX" rt (list bufnum degree index) #f 1 #f #f))))
(define RTraceRdY
  (lambda (rt bufnum degree index)
    (mk-ugen (list "RTraceRdY" rt (list bufnum degree index) #f 1 #f #f))))
(define RTraceRdZ
  (lambda (rt bufnum degree index)
    (mk-ugen (list "RTraceRdZ" rt (list bufnum degree index) #f 1 #f #f))))
(define RadiansPerSample
  (lambda (rt)
    (mk-ugen (list "RadiansPerSample" rt (list ) #f 1 #f #f))))
(define Ramp
  (lambda (in lagTime)
    (mk-ugen (list "Ramp" #f (list in lagTime) #f 1 #f #f))))
(define Rand
  (lambda (rt lo hi)
    (mk-ugen (list "Rand" rt (list lo hi) #f 1 #f (incr-uid)))))
(define RandID
  (lambda (rt id)
    (mk-ugen (list "RandID" rt (list id) #f 1 #f #f))))
(define RandSeed
  (lambda (rt trig seed)
    (mk-ugen (list "RandSeed" rt (list trig seed) #f 1 #f #f))))
(define RecordBuf
  (lambda (rt inputArray bufnum offset recLevel preLevel run loop trigger doneAction)
    (mk-ugen (list "RecordBuf" rt (list inputArray bufnum offset recLevel preLevel run loop trigger) doneAction 1 #f #f))))
(define ReplaceOut
  (lambda (bus channelsArray)
    (mk-ugen (list "ReplaceOut" #f (list bus) channelsArray 1 #f #f))))
(define Resonz
  (lambda (in freq bwr)
    (mk-ugen (list "Resonz" #f (list in freq bwr) #f 1 #f #f))))
(define Ringz
  (lambda (in freq decaytime)
    (mk-ugen (list "Ringz" #f (list in freq decaytime) #f 1 #f #f))))
(define Rotate2
  (lambda (rt x y pos)
    (mk-ugen (list "Rotate2" rt (list x y pos) #f 2 #f #f))))
(define RunningMax
  (lambda (in trig)
    (mk-ugen (list "RunningMax" #f (list in trig) #f 1 #f #f))))
(define RunningMin
  (lambda (in trig)
    (mk-ugen (list "RunningMin" #f (list in trig) #f 1 #f #f))))
(define RunningSum
  (lambda (in numsamp)
    (mk-ugen (list "RunningSum" #f (list in numsamp) #f 1 #f #f))))
(define SOS
  (lambda (in a0 a1 a2 b1 b2)
    (mk-ugen (list "SOS" #f (list in a0 a1 a2 b1 b2) #f 1 #f #f))))
(define SampleDur
  (lambda (rt)
    (mk-ugen (list "SampleDur" rt (list ) #f 1 #f #f))))
(define SampleRate
  (lambda (rt)
    (mk-ugen (list "SampleRate" rt (list ) #f 1 #f #f))))
(define Saw
  (lambda (rt freq)
    (mk-ugen (list "Saw" rt (list freq) #f 1 #f #f))))
(define Schmidt
  (lambda (rt in lo hi)
    (mk-ugen (list "Schmidt" rt (list in lo hi) #f 1 #f #f))))
(define ScopeOut
  (lambda (rt inputArray bufnum)
    (mk-ugen (list "ScopeOut" rt (list inputArray bufnum) #f 1 #f #f))))
(define ScopeOut2
  (lambda (rt inputArray scopeNum maxFrames scopeFrames)
    (mk-ugen (list "ScopeOut2" rt (list inputArray scopeNum maxFrames scopeFrames) #f 1 #f #f))))
(define Select
  (lambda (which array)
    (mk-ugen (list "Select" #f (list which) array 1 #f #f))))
(define SendTrig
  (lambda (in id value)
    (mk-ugen (list "SendTrig" #f (list in id value) #f 1 #f #f))))
(define SetBuf
  (lambda (rt buf numValues offset values)
    (mk-ugen (list "SetBuf" rt (list buf numValues offset) values 1 #f #f))))
(define SetResetFF
  (lambda (trig reset)
    (mk-ugen (list "SetResetFF" #f (list trig reset) #f 1 #f #f))))
(define Shaper
  (lambda (bufnum in)
    (mk-ugen (list "Shaper" #f (list bufnum in) #f 1 #f #f))))
(define SinOsc
  (lambda (rt freq phase)
    (mk-ugen (list "SinOsc" rt (list freq phase) #f 1 #f #f))))
(define SinOscFB
  (lambda (rt freq feedback)
    (mk-ugen (list "SinOscFB" rt (list freq feedback) #f 1 #f #f))))
(define Slew
  (lambda (in up dn)
    (mk-ugen (list "Slew" #f (list in up dn) #f 1 #f #f))))
(define Slope
  (lambda (rt in)
    (mk-ugen (list "Slope" rt (list in) #f 1 #f #f))))
(define SpecCentroid
  (lambda (rt buffer)
    (mk-ugen (list "SpecCentroid" rt (list buffer) #f 1 #f #f))))
(define SpecFlatness
  (lambda (rt buffer)
    (mk-ugen (list "SpecFlatness" rt (list buffer) #f 1 #f #f))))
(define SpecPcile
  (lambda (rt buffer fraction interpolate)
    (mk-ugen (list "SpecPcile" rt (list buffer fraction interpolate) #f 1 #f #f))))
(define Splay
  (lambda (rt inArray spread level center levelComp)
    (mk-ugen (list "Splay" rt (list inArray spread level center levelComp) #f 2 #f #f))))
(define SplayAz
  (lambda (rt inArray spread level width center orientation levelComp)
    (mk-ugen (list "SplayAz" rt (list inArray spread level width center orientation levelComp) #f 1 #f #f))))
(define Spring
  (lambda (rt in spring damp)
    (mk-ugen (list "Spring" rt (list in spring damp) #f 1 #f #f))))
(define StandardL
  (lambda (rt freq k xi yi)
    (mk-ugen (list "StandardL" rt (list freq k xi yi) #f 1 #f #f))))
(define StandardN
  (lambda (rt freq k xi yi)
    (mk-ugen (list "StandardN" rt (list freq k xi yi) #f 1 #f #f))))
(define Stepper
  (lambda (trig reset min max step resetval)
    (mk-ugen (list "Stepper" #f (list trig reset min max step resetval) #f 1 #f #f))))
(define StereoConvolution2L
  (lambda (rt in kernelL kernelR trigger framesize crossfade)
    (mk-ugen (list "StereoConvolution2L" rt (list in kernelL kernelR trigger framesize crossfade) #f 2 #f #f))))
(define SubsampleOffset
  (lambda (rt)
    (mk-ugen (list "SubsampleOffset" rt (list ) #f 1 #f #f))))
(define Sum3
  (lambda (rt in0 in1 in2)
    (mk-ugen (list "Sum3" rt (list in0 in1 in2) #f 1 #f #f))))
(define Sum4
  (lambda (rt in0 in1 in2 in3)
    (mk-ugen (list "Sum4" rt (list in0 in1 in2 in3) #f 1 #f #f))))
(define Sweep
  (lambda (trig rate)
    (mk-ugen (list "Sweep" #f (list trig rate) #f 1 #f #f))))
(define SyncSaw
  (lambda (rt syncFreq sawFreq)
    (mk-ugen (list "SyncSaw" rt (list syncFreq sawFreq) #f 1 #f #f))))
(define T2A
  (lambda (rt in offset)
    (mk-ugen (list "T2A" rt (list in offset) #f 1 #f #f))))
(define T2K
  (lambda (rt in)
    (mk-ugen (list "T2K" rt (list in) #f 1 #f #f))))
(define TBall
  (lambda (rt in g damp friction)
    (mk-ugen (list "TBall" rt (list in g damp friction) #f 1 #f #f))))
(define TDelay
  (lambda (in dur)
    (mk-ugen (list "TDelay" #f (list in dur) #f 1 #f #f))))
(define TDuty
  (lambda (rt dur reset level doneAction gapFirst)
    (mk-ugen (list "TDuty" rt (list dur reset level doneAction gapFirst) #f 1 #f #f))))
(define TExpRand
  (lambda (rt lo hi trig)
    (mk-ugen (list "TExpRand" rt (list lo hi trig) #f 1 #f (incr-uid)))))
(define TGrains
  (lambda (nc rt trigger bufnum rate centerPos dur pan amp interp)
    (mk-ugen (list "TGrains" rt (list trigger bufnum rate centerPos dur pan amp interp) #f nc #f #f))))
(define TIRand
  (lambda (rt lo hi trig)
    (mk-ugen (list "TIRand" rt (list lo hi trig) #f 1 #f (incr-uid)))))
(define TRand
  (lambda (rt lo hi trig)
    (mk-ugen (list "TRand" rt (list lo hi trig) #f 1 #f (incr-uid)))))
(define TWindex
  (lambda (rt in array normalize)
    (mk-ugen (list "TWindex" rt (list in array) normalize 1 #f (incr-uid)))))
(define Tap
  (lambda (nc rt bufnum delaytime)
    (mk-ugen (list "Tap" rt (list bufnum delaytime) #f nc #f #f))))
(define Timer
  (lambda (trig)
    (mk-ugen (list "Timer" #f (list trig) #f 1 #f #f))))
(define ToggleFF
  (lambda (trig)
    (mk-ugen (list "ToggleFF" #f (list trig) #f 1 #f #f))))
(define Trig
  (lambda (in dur)
    (mk-ugen (list "Trig" #f (list in dur) #f 1 #f #f))))
(define Trig1
  (lambda (in dur)
    (mk-ugen (list "Trig1" #f (list in dur) #f 1 #f #f))))
(define TrigControl
  (lambda (rt values)
    (mk-ugen (list "TrigControl" rt (list values) #f 1 #f #f))))
(define TwoPole
  (lambda (in freq radius)
    (mk-ugen (list "TwoPole" #f (list in freq radius) #f 1 #f #f))))
(define TwoZero
  (lambda (in freq radius)
    (mk-ugen (list "TwoZero" #f (list in freq radius) #f 1 #f #f))))
(define UnaryOpUGen
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 #f #f))))
(define VDiskIn
  (lambda (nc rt bufnum rate loop sendID)
    (mk-ugen (list "VDiskIn" rt (list bufnum rate loop sendID) #f nc #f #f))))
(define VOsc
  (lambda (rt bufpos freq phase)
    (mk-ugen (list "VOsc" rt (list bufpos freq phase) #f 1 #f #f))))
(define VOsc3
  (lambda (rt bufpos freq1 freq2 freq3)
    (mk-ugen (list "VOsc3" rt (list bufpos freq1 freq2 freq3) #f 1 #f #f))))
(define VarLag
  (lambda (rt in time curvature warp start)
    (mk-ugen (list "VarLag" rt (list in time curvature warp start) #f 1 #f #f))))
(define VarSaw
  (lambda (rt freq iphase width)
    (mk-ugen (list "VarSaw" rt (list freq iphase width) #f 1 #f #f))))
(define Vibrato
  (lambda (rt freq rate depth delay onset rateVariation depthVariation iphase)
    (mk-ugen (list "Vibrato" rt (list freq rate depth delay onset rateVariation depthVariation iphase) #f 1 #f (incr-uid)))))
(define Warp1
  (lambda (nc rt bufnum pointer freqScale windowSize envbufnum overlaps windowRandRatio interp)
    (mk-ugen (list "Warp1" rt (list bufnum pointer freqScale windowSize envbufnum overlaps windowRandRatio interp) #f nc #f #f))))
(define WhiteNoise
  (lambda (rt)
    (mk-ugen (list "WhiteNoise" rt (list ) #f 1 #f (incr-uid)))))
(define WidthFirstUGen
  (lambda (rt maxSize)
    (mk-ugen (list "WidthFirstUGen" rt (list maxSize) #f 1 #f #f))))
(define Wrap
  (lambda (in lo hi)
    (mk-ugen (list "Wrap" #f (list in lo hi) #f 1 #f #f))))
(define WrapIndex
  (lambda (bufnum in)
    (mk-ugen (list "WrapIndex" #f (list bufnum in) #f 1 #f #f))))
(define XFade2
  (lambda (rt inA inB pan level)
    (mk-ugen (list "XFade2" rt (list inA inB pan level) #f 1 #f #f))))
(define XLine
  (lambda (rt start end dur doneAction)
    (mk-ugen (list "XLine" rt (list start end dur doneAction) #f 1 #f #f))))
(define XOut
  (lambda (bus xfade channelsArray)
    (mk-ugen (list "XOut" #f (list bus xfade) channelsArray 1 #f #f))))
(define ZeroCrossing
  (lambda (rt in)
    (mk-ugen (list "ZeroCrossing" rt (list in) #f 1 #f #f))))
