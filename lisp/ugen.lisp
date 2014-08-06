(define local-buf
  (lambda (rt numChannels numFrames)
    (mk-ugen (list "LocalBuf" rt (list numChannels numFrames) nil 1 nil (incr-uid 1)))))
(define mul-add
  (lambda (in mul add)
    (mk-ugen (list "MulAdd" (list 0) (list in mul add) nil 1 nil nil))))
(define a2k
  (lambda (rt in)
    (mk-ugen (list "A2K" rt (list in) nil 1 nil nil))))
(define apf
  (lambda (rt in freq radius)
    (mk-ugen (list "APF" rt (list in freq radius) nil 1 nil nil))))
(define allpass-c
  (lambda (in maxdelaytime delaytime decaytime)
    (mk-ugen (list "AllpassC" (list 0) (list in maxdelaytime delaytime decaytime) nil 1 nil nil))))
(define allpass-l
  (lambda (in maxdelaytime delaytime decaytime)
    (mk-ugen (list "AllpassL" (list 0) (list in maxdelaytime delaytime decaytime) nil 1 nil nil))))
(define allpass-n
  (lambda (in maxdelaytime delaytime decaytime)
    (mk-ugen (list "AllpassN" (list 0) (list in maxdelaytime delaytime decaytime) nil 1 nil nil))))
(define amp-comp
  (lambda (rt freq root exp)
    (mk-ugen (list "AmpComp" rt (list freq root exp) nil 1 nil nil))))
(define amp-comp-a
  (lambda (rt freq root minAmp rootAmp)
    (mk-ugen (list "AmpCompA" rt (list freq root minAmp rootAmp) nil 1 nil nil))))
(define amplitude
  (lambda (rt in attackTime releaseTime)
    (mk-ugen (list "Amplitude" rt (list in attackTime releaseTime) nil 1 nil nil))))
(define audio-control
  (lambda (rt values)
    (mk-ugen (list "AudioControl" rt (list values) nil 1 nil nil))))
(define b-all-pass
  (lambda (in freq rq)
    (mk-ugen (list "BAllPass" (list 0) (list in freq rq) nil 1 nil nil))))
(define b-band-pass
  (lambda (in freq bw)
    (mk-ugen (list "BBandPass" (list 0) (list in freq bw) nil 1 nil nil))))
(define b-band-stop
  (lambda (in freq bw)
    (mk-ugen (list "BBandStop" (list 0) (list in freq bw) nil 1 nil nil))))
(define b-hi-pass
  (lambda (in freq rq)
    (mk-ugen (list "BHiPass" (list 0) (list in freq rq) nil 1 nil nil))))
(define b-hi-shelf
  (lambda (in freq rs db)
    (mk-ugen (list "BHiShelf" (list 0) (list in freq rs db) nil 1 nil nil))))
(define b-low-pass
  (lambda (in freq rq)
    (mk-ugen (list "BLowPass" (list 0) (list in freq rq) nil 1 nil nil))))
(define b-low-shelf
  (lambda (in freq rs db)
    (mk-ugen (list "BLowShelf" (list 0) (list in freq rs db) nil 1 nil nil))))
(define bpf
  (lambda (in freq rq)
    (mk-ugen (list "BPF" (list 0) (list in freq rq) nil 1 nil nil))))
(define bpz2
  (lambda (in)
    (mk-ugen (list "BPZ2" (list 0) (list in) nil 1 nil nil))))
(define b-peak-eq
  (lambda (in freq rq db)
    (mk-ugen (list "BPeakEQ" (list 0) (list in freq rq db) nil 1 nil nil))))
(define brf
  (lambda (in freq rq)
    (mk-ugen (list "BRF" (list 0) (list in freq rq) nil 1 nil nil))))
(define brz2
  (lambda (in)
    (mk-ugen (list "BRZ2" (list 0) (list in) nil 1 nil nil))))
(define balance2
  (lambda (rt left right pos level)
    (mk-ugen (list "Balance2" rt (list left right pos level) nil 2 nil nil))))
(define ball
  (lambda (rt in g damp friction)
    (mk-ugen (list "Ball" rt (list in g damp friction) nil 1 nil nil))))
(define beat-track
  (lambda (rt chain lock)
    (mk-ugen (list "BeatTrack" rt (list chain lock) nil 1 nil nil))))
(define beat-track2
  (lambda (rt busindex numfeatures windowsize phaseaccuracy lock weightingscheme)
    (mk-ugen (list "BeatTrack2" rt (list busindex numfeatures windowsize phaseaccuracy lock weightingscheme) nil 6 nil nil))))
(define bi-pan-b2
  (lambda (rt inA inB azimuth gain)
    (mk-ugen (list "BiPanB2" rt (list inA inB azimuth gain) nil 3 nil nil))))
(define binary-op-ugen
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 nil nil))))
(define blip
  (lambda (rt freq numharm)
    (mk-ugen (list "Blip" rt (list freq numharm) nil 1 nil nil))))
(define block-size
  (lambda (rt)
    (mk-ugen (list "BlockSize" rt nil nil 1 nil nil))))
(define brown-noise
  (lambda (rt)
    (mk-ugen (list "BrownNoise" rt nil nil 1 nil (incr-uid 1)))))
(define buf-allpass-c
  (lambda (rt buf in delaytime decaytime)
    (mk-ugen (list "BufAllpassC" rt (list buf in delaytime decaytime) nil 1 nil nil))))
(define buf-allpass-l
  (lambda (rt buf in delaytime decaytime)
    (mk-ugen (list "BufAllpassL" rt (list buf in delaytime decaytime) nil 1 nil nil))))
(define buf-allpass-n
  (lambda (rt buf in delaytime decaytime)
    (mk-ugen (list "BufAllpassN" rt (list buf in delaytime decaytime) nil 1 nil nil))))
(define buf-channels
  (lambda (rt bufnum)
    (mk-ugen (list "BufChannels" rt (list bufnum) nil 1 nil nil))))
(define buf-comb-c
  (lambda (rt buf in delaytime decaytime)
    (mk-ugen (list "BufCombC" rt (list buf in delaytime decaytime) nil 1 nil nil))))
(define buf-comb-l
  (lambda (rt buf in delaytime decaytime)
    (mk-ugen (list "BufCombL" rt (list buf in delaytime decaytime) nil 1 nil nil))))
(define buf-comb-n
  (lambda (rt buf in delaytime decaytime)
    (mk-ugen (list "BufCombN" rt (list buf in delaytime decaytime) nil 1 nil nil))))
(define buf-delay-c
  (lambda (rt buf in delaytime)
    (mk-ugen (list "BufDelayC" rt (list buf in delaytime) nil 1 nil nil))))
(define buf-delay-l
  (lambda (rt buf in delaytime)
    (mk-ugen (list "BufDelayL" rt (list buf in delaytime) nil 1 nil nil))))
(define buf-delay-n
  (lambda (rt buf in delaytime)
    (mk-ugen (list "BufDelayN" rt (list buf in delaytime) nil 1 nil nil))))
(define buf-dur
  (lambda (rt bufnum)
    (mk-ugen (list "BufDur" rt (list bufnum) nil 1 nil nil))))
(define buf-frames
  (lambda (rt bufnum)
    (mk-ugen (list "BufFrames" rt (list bufnum) nil 1 nil nil))))
(define buf-info-ugen-base
  (lambda (rt bufnum)
    (mk-ugen (list "BufInfoUGenBase" rt (list bufnum) nil 1 nil nil))))
(define buf-rate-scale
  (lambda (rt bufnum)
    (mk-ugen (list "BufRateScale" rt (list bufnum) nil 1 nil nil))))
(define buf-rd
  (lambda (nc rt bufnum phase loop interpolation)
    (mk-ugen (list "BufRd" rt (list bufnum phase loop interpolation) nil nc nil nil))))
(define buf-sample-rate
  (lambda (rt bufnum)
    (mk-ugen (list "BufSampleRate" rt (list bufnum) nil 1 nil nil))))
(define buf-samples
  (lambda (rt bufnum)
    (mk-ugen (list "BufSamples" rt (list bufnum) nil 1 nil nil))))
(define buf-wr
  (lambda (rt inputArray bufnum phase loop)
    (mk-ugen (list "BufWr" rt (list inputArray bufnum phase) loop 1 nil nil))))
(define c-osc
  (lambda (rt bufnum freq beats)
    (mk-ugen (list "COsc" rt (list bufnum freq beats) nil 1 nil nil))))
(define changed
  (lambda (rt input threshold)
    (mk-ugen (list "Changed" rt (list input threshold) nil 1 nil nil))))
(define check-bad-values
  (lambda (rt in id post)
    (mk-ugen (list "CheckBadValues" rt (list in id post) nil 1 nil nil))))
(define clip
  (lambda (in lo hi)
    (mk-ugen (list "Clip" (list 0) (list in lo hi) nil 1 nil nil))))
(define clip-noise
  (lambda (rt)
    (mk-ugen (list "ClipNoise" rt nil nil 1 nil (incr-uid 1)))))
(define coin-gate
  (lambda (prob in)
    (mk-ugen (list "CoinGate" (list 1) (list prob in) nil 1 nil (incr-uid 1)))))
(define comb-c
  (lambda (in maxdelaytime delaytime decaytime)
    (mk-ugen (list "CombC" (list 0) (list in maxdelaytime delaytime decaytime) nil 1 nil nil))))
(define comb-l
  (lambda (in maxdelaytime delaytime decaytime)
    (mk-ugen (list "CombL" (list 0) (list in maxdelaytime delaytime decaytime) nil 1 nil nil))))
(define comb-n
  (lambda (in maxdelaytime delaytime decaytime)
    (mk-ugen (list "CombN" (list 0) (list in maxdelaytime delaytime decaytime) nil 1 nil nil))))
(define compander
  (lambda (in control thresh slopeBelow slopeAbove clampTime relaxTime)
    (mk-ugen (list "Compander" (list 0) (list in control thresh slopeBelow slopeAbove clampTime relaxTime) nil 1 nil nil))))
(define compander-d
  (lambda (rt in thresh slopeBelow slopeAbove clampTime relaxTime)
    (mk-ugen (list "CompanderD" rt (list in thresh slopeBelow slopeAbove clampTime relaxTime) nil 1 nil nil))))
(define control
  (lambda (rt values)
    (mk-ugen (list "Control" rt (list values) nil 1 nil nil))))
(define control-dur (mk-ugen (list "ControlDur" ir nil nil 1 nil nil)))
(define control-rate (mk-ugen (list "ControlRate" ir nil nil 1 nil nil)))
(define convolution
  (lambda (rt in kernel framesize)
    (mk-ugen (list "Convolution" rt (list in kernel framesize) nil 1 nil nil))))
(define convolution2
  (lambda (rt in kernel trigger framesize)
    (mk-ugen (list "Convolution2" rt (list in kernel trigger framesize) nil 1 nil nil))))
(define convolution2l
  (lambda (rt in kernel trigger framesize crossfade)
    (mk-ugen (list "Convolution2L" rt (list in kernel trigger framesize crossfade) nil 1 nil nil))))
(define convolution3
  (lambda (rt in kernel trigger framesize)
    (mk-ugen (list "Convolution3" rt (list in kernel trigger framesize) nil 1 nil nil))))
(define crackle
  (lambda (rt chaosParam)
    (mk-ugen (list "Crackle" rt (list chaosParam) nil 1 nil nil))))
(define cusp-l
  (lambda (rt freq a b xi)
    (mk-ugen (list "CuspL" rt (list freq a b xi) nil 1 nil nil))))
(define cusp-n
  (lambda (rt freq a b xi)
    (mk-ugen (list "CuspN" rt (list freq a b xi) nil 1 nil nil))))
(define dc
  (lambda (rt in)
    (mk-ugen (list "DC" rt (list in) nil 1 nil nil))))
(define dbrown
  (lambda (lo hi step length)
    (mk-ugen (list "Dbrown" dr (list lo hi step length) nil 1 nil nil))))
(define dbufrd
  (lambda (bufnum phase loop)
    (mk-ugen (list "Dbufrd" dr (list bufnum phase loop) nil 1 nil nil))))
(define dbufwr
  (lambda (input bufnum phase loop)
    (mk-ugen (list "Dbufwr" dr (list input bufnum phase loop) nil 1 nil nil))))
(define decay
  (lambda (in decayTime)
    (mk-ugen (list "Decay" (list 0) (list in decayTime) nil 1 nil nil))))
(define decay2
  (lambda (in attackTime decayTime)
    (mk-ugen (list "Decay2" (list 0) (list in attackTime decayTime) nil 1 nil nil))))
(define decode-b2
  (lambda (nc rt w x y orientation)
    (mk-ugen (list "DecodeB2" rt (list w x y orientation) nil nc nil nil))))
(define degree-to-key
  (lambda (bufnum in octave)
    (mk-ugen (list "DegreeToKey" (list 1) (list bufnum in octave) nil 1 nil nil))))
(define del-tap-rd
  (lambda (rt buffer phase delTime interp)
    (mk-ugen (list "DelTapRd" rt (list buffer phase delTime interp) nil 1 nil nil))))
(define del-tap-wr
  (lambda (rt buffer in)
    (mk-ugen (list "DelTapWr" rt (list buffer in) nil 1 nil nil))))
(define delay1
  (lambda (in)
    (mk-ugen (list "Delay1" (list 0) (list in) nil 1 nil nil))))
(define delay2
  (lambda (in)
    (mk-ugen (list "Delay2" (list 0) (list in) nil 1 nil nil))))
(define delay-c
  (lambda (in maxdelaytime delaytime)
    (mk-ugen (list "DelayC" (list 0) (list in maxdelaytime delaytime) nil 1 nil nil))))
(define delay-l
  (lambda (in maxdelaytime delaytime)
    (mk-ugen (list "DelayL" (list 0) (list in maxdelaytime delaytime) nil 1 nil nil))))
(define delay-n
  (lambda (in maxdelaytime delaytime)
    (mk-ugen (list "DelayN" (list 0) (list in maxdelaytime delaytime) nil 1 nil nil))))
(define demand
  (lambda (trig reset demandUGens)
    (mk-ugen (list "Demand" (list 0) (list trig reset demandUGens) nil 1 nil nil))))
(define demand-env-gen
  (lambda (rt level dur shape curve gate reset levelScale levelBias timeScale doneAction)
    (mk-ugen (list "DemandEnvGen" rt (list level dur shape curve gate reset levelScale levelBias timeScale doneAction) nil 1 nil nil))))
(define detect-index
  (lambda (rt bufnum in)
    (mk-ugen (list "DetectIndex" rt (list bufnum in) nil 1 nil nil))))
(define detect-silence
  (lambda (rt in amp time doneAction)
    (mk-ugen (list "DetectSilence" rt (list in amp time doneAction) nil 1 nil nil))))
(define dgeom
  (lambda (start grow length)
    (mk-ugen (list "Dgeom" dr (list start grow length) nil 1 nil (incr-uid 1)))))
(define dibrown
  (lambda (lo hi step length)
    (mk-ugen (list "Dibrown" dr (list lo hi step length) nil 1 nil (incr-uid 1)))))
(define disk-in
  (lambda (nc rt bufnum loop)
    (mk-ugen (list "DiskIn" rt (list bufnum loop) nil nc nil nil))))
(define disk-out
  (lambda (rt bufnum channelsArray)
    (mk-ugen (list "DiskOut" rt (list bufnum) channelsArray 1 nil nil))))
(define diwhite
  (lambda (lo hi length)
    (mk-ugen (list "Diwhite" dr (list lo hi length) nil 1 nil (incr-uid 1)))))
(define donce
  (lambda (in)
    (mk-ugen (list "Donce" dr (list in) nil 1 nil (incr-uid 1)))))
(define done
  (lambda (rt src)
    (mk-ugen (list "Done" rt (list src) nil 1 nil nil))))
(define dpoll
  (lambda (in label run trigid)
    (mk-ugen (list "Dpoll" dr (list in label run trigid) nil 1 nil (incr-uid 1)))))
(define drand
  (lambda (list_ repeats)
    (mk-ugen (list "Drand" dr (list list_) repeats 1 nil (incr-uid 1)))))
(define dreset
  (lambda (in reset)
    (mk-ugen (list "Dreset" dr (list in reset) nil 1 nil nil))))
(define dseq
  (lambda (list_ repeats)
    (mk-ugen (list "Dseq" dr (list list_) repeats 1 nil (incr-uid 1)))))
(define dser
  (lambda (list_ repeats)
    (mk-ugen (list "Dser" dr (list list_) repeats 1 nil (incr-uid 1)))))
(define dseries
  (lambda (start step length)
    (mk-ugen (list "Dseries" dr (list start step length) nil 1 nil (incr-uid 1)))))
(define dshuf
  (lambda (list_ repeats)
    (mk-ugen (list "Dshuf" dr (list list_ repeats) nil 1 nil (incr-uid 1)))))
(define dstutter
  (lambda (n in)
    (mk-ugen (list "Dstutter" dr (list n in) nil 1 nil nil))))
(define dswitch
  (lambda (list_ index)
    (mk-ugen (list "Dswitch" dr (list list_) index 1 nil (incr-uid 1)))))
(define dswitch1
  (lambda (list_ index)
    (mk-ugen (list "Dswitch1" dr (list list_) index 1 nil (incr-uid 1)))))
(define dunique
  (lambda (source maxBufferSize protected)
    (mk-ugen (list "Dunique" dr (list source maxBufferSize protected) nil 1 nil nil))))
(define dust
  (lambda (rt density)
    (mk-ugen (list "Dust" rt (list density) nil 1 nil (incr-uid 1)))))
(define dust2
  (lambda (rt density)
    (mk-ugen (list "Dust2" rt (list density) nil 1 nil (incr-uid 1)))))
(define dust-r
  (lambda (rt iot_min iot_max)
    (mk-ugen (list "DustR" rt (list iot_min iot_max) nil 1 nil nil))))
(define duty
  (lambda (rt dur reset level doneAction)
    (mk-ugen (list "Duty" rt (list dur reset level doneAction) nil 1 nil nil))))
(define dwhite
  (lambda (lo hi length)
    (mk-ugen (list "Dwhite" dr (list lo hi length) nil 1 nil nil))))
(define dwrand
  (lambda (list_ weights repeats)
    (mk-ugen (list "Dwrand" dr (list list_ weights repeats) nil 1 nil (incr-uid 1)))))
(define dxrand
  (lambda (list_ repeats)
    (mk-ugen (list "Dxrand" dr (list list_) repeats 1 nil (incr-uid 1)))))
(define env-gen
  (lambda (rt envelope gate levelScale levelBias timeScale doneAction)
    (mk-ugen (list "EnvGen" rt (list envelope gate levelScale levelBias timeScale) doneAction 1 nil nil))))
(define exp-rand
  (lambda (lo hi)
    (mk-ugen (list "ExpRand" (list 0 1) (list lo hi) nil 1 nil (incr-uid 1)))))
(define fb-sine-c
  (lambda (rt freq im fb a c xi yi)
    (mk-ugen (list "FBSineC" rt (list freq im fb a c xi yi) nil 1 nil nil))))
(define fb-sine-l
  (lambda (rt freq im fb a c xi yi)
    (mk-ugen (list "FBSineL" rt (list freq im fb a c xi yi) nil 1 nil nil))))
(define fb-sine-n
  (lambda (rt freq im fb a c xi yi)
    (mk-ugen (list "FBSineN" rt (list freq im fb a c xi yi) nil 1 nil nil))))
(define fft
  (lambda (rt buffer in hop wintype active winsize)
    (mk-ugen (list "FFT" rt (list buffer in hop wintype active winsize) nil 1 nil nil))))
(define fos
  (lambda (in a0 a1 b1)
    (mk-ugen (list "FOS" (list 0) (list in a0 a1 b1) nil 1 nil nil))))
(define f-sin-osc
  (lambda (rt freq iphase)
    (mk-ugen (list "FSinOsc" rt (list freq iphase) nil 1 nil nil))))
(define fold
  (lambda (in lo hi)
    (mk-ugen (list "Fold" (list 0) (list in lo hi) nil 1 nil nil))))
(define formant
  (lambda (rt fundfreq formfreq bwfreq)
    (mk-ugen (list "Formant" rt (list fundfreq formfreq bwfreq) nil 1 nil nil))))
(define formlet
  (lambda (in freq attacktime decaytime)
    (mk-ugen (list "Formlet" (list 0) (list in freq attacktime decaytime) nil 1 nil nil))))
(define free
  (lambda (rt trig id)
    (mk-ugen (list "Free" rt (list trig id) nil 1 nil nil))))
(define free-self
  (lambda (rt in)
    (mk-ugen (list "FreeSelf" rt (list in) nil 1 nil nil))))
(define free-self-when-done
  (lambda (rt src)
    (mk-ugen (list "FreeSelfWhenDone" rt (list src) nil 1 nil nil))))
(define free-verb
  (lambda (in mix room damp)
    (mk-ugen (list "FreeVerb" (list 0) (list in mix room damp) nil 1 nil nil))))
(define free-verb2
  (lambda (in in2 mix room damp)
    (mk-ugen (list "FreeVerb2" (list 0) (list in in2 mix room damp) nil 2 nil nil))))
(define freq-shift
  (lambda (rt in freq phase)
    (mk-ugen (list "FreqShift" rt (list in freq phase) nil 1 nil nil))))
(define g-verb
  (lambda (in roomsize revtime damping inputbw spread drylevel earlyreflevel taillevel maxroomsize)
    (mk-ugen (list "GVerb" (list 0) (list in roomsize revtime damping inputbw spread drylevel earlyreflevel taillevel maxroomsize) nil 2 nil nil))))
(define gate
  (lambda (in trig)
    (mk-ugen (list "Gate" (list 0) (list in trig) nil 1 nil nil))))
(define gbman-l
  (lambda (rt freq xi yi)
    (mk-ugen (list "GbmanL" rt (list freq xi yi) nil 1 nil nil))))
(define gbman-n
  (lambda (rt freq xi yi)
    (mk-ugen (list "GbmanN" rt (list freq xi yi) nil 1 nil nil))))
(define gendy1
  (lambda (rt ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum)
    (mk-ugen (list "Gendy1" rt (list ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum) nil 1 nil (incr-uid 1)))))
(define gendy2
  (lambda (rt ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum a c)
    (mk-ugen (list "Gendy2" rt (list ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum a c) nil 1 nil (incr-uid 1)))))
(define gendy3
  (lambda (rt ampdist durdist adparam ddparam freq ampscale durscale initCPs knum)
    (mk-ugen (list "Gendy3" rt (list ampdist durdist adparam ddparam freq ampscale durscale initCPs knum) nil 1 nil (incr-uid 1)))))
(define grain-buf
  (lambda (nc trigger dur sndbuf rate pos interp pan envbufnum maxGrains)
    (mk-ugen (list "GrainBuf" ar (list trigger dur sndbuf rate pos interp pan envbufnum maxGrains) nil nc nil nil))))
(define grain-fm
  (lambda (nc trigger dur carfreq modfreq index pan envbufnum maxGrains)
    (mk-ugen (list "GrainFM" ar (list trigger dur carfreq modfreq index pan envbufnum maxGrains) nil nc nil nil))))
(define grain-in
  (lambda (nc trigger dur in pan envbufnum maxGrains)
    (mk-ugen (list "GrainIn" ar (list trigger dur in pan envbufnum maxGrains) nil nc nil nil))))
(define grain-sin
  (lambda (nc trigger dur freq pan envbufnum maxGrains)
    (mk-ugen (list "GrainSin" ar (list trigger dur freq pan envbufnum maxGrains) nil nc nil nil))))
(define gray-noise
  (lambda (rt)
    (mk-ugen (list "GrayNoise" rt nil nil 1 nil (incr-uid 1)))))
(define hpf
  (lambda (in freq)
    (mk-ugen (list "HPF" (list 0) (list in freq) nil 1 nil nil))))
(define hpz1
  (lambda (in)
    (mk-ugen (list "HPZ1" (list 0) (list in) nil 1 nil nil))))
(define hpz2
  (lambda (in)
    (mk-ugen (list "HPZ2" (list 0) (list in) nil 1 nil nil))))
(define hasher
  (lambda (in)
    (mk-ugen (list "Hasher" (list 0) (list in) nil 1 nil nil))))
(define henon-c
  (lambda (rt freq a b x0 x1)
    (mk-ugen (list "HenonC" rt (list freq a b x0 x1) nil 1 nil nil))))
(define henon-l
  (lambda (rt freq a b x0 x1)
    (mk-ugen (list "HenonL" rt (list freq a b x0 x1) nil 1 nil nil))))
(define henon-n
  (lambda (rt freq a b x0 x1)
    (mk-ugen (list "HenonN" rt (list freq a b x0 x1) nil 1 nil nil))))
(define hilbert
  (lambda (in)
    (mk-ugen (list "Hilbert" (list 0) (list in) nil 2 nil nil))))
(define hilbert-fir
  (lambda (rt in buffer)
    (mk-ugen (list "HilbertFIR" rt (list in buffer) nil 2 nil nil))))
(define i-env-gen
  (lambda (rt envelope index)
    (mk-ugen (list "IEnvGen" rt (list envelope index) nil 1 nil nil))))
(define ifft
  (lambda (rt buffer wintype winsize)
    (mk-ugen (list "IFFT" rt (list buffer wintype winsize) nil 1 nil nil))))
(define i-rand
  (lambda (lo hi)
    (mk-ugen (list "IRand" (list 0 1) (list lo hi) nil 1 nil (incr-uid 1)))))
(define impulse
  (lambda (rt freq phase)
    (mk-ugen (list "Impulse" rt (list freq phase) nil 1 nil nil))))
(define in
  (lambda (nc rt bus)
    (mk-ugen (list "In" rt (list bus) nil nc nil nil))))
(define in-bus
  (lambda (rt bus offset clip)
    (mk-ugen (list "InBus" rt (list bus offset clip) nil 1 nil nil))))
(define in-feedback
  (lambda (nc rt bus)
    (mk-ugen (list "InFeedback" rt (list bus) nil nc nil nil))))
(define in-range
  (lambda (in lo hi)
    (mk-ugen (list "InRange" (list 0) (list in lo hi) nil 1 nil nil))))
(define in-rect
  (lambda (rt x y rect)
    (mk-ugen (list "InRect" rt (list x y rect) nil 1 nil nil))))
(define in-trig
  (lambda (nc rt bus)
    (mk-ugen (list "InTrig" rt (list bus) nil nc nil nil))))
(define index
  (lambda (rt bufnum in)
    (mk-ugen (list "Index" rt (list bufnum in) nil 1 nil nil))))
(define index-in-between
  (lambda (rt bufnum in)
    (mk-ugen (list "IndexInBetween" rt (list bufnum in) nil 1 nil nil))))
(define index-l
  (lambda (rt bufnum in)
    (mk-ugen (list "IndexL" rt (list bufnum in) nil 1 nil nil))))
(define info-ugen-base
  (lambda (rt)
    (mk-ugen (list "InfoUGenBase" rt nil nil 1 nil nil))))
(define integrator
  (lambda (in coef)
    (mk-ugen (list "Integrator" (list 0) (list in coef) nil 1 nil nil))))
(define k2a
  (lambda (in)
    (mk-ugen (list "K2A" ar (list in) nil 1 nil nil))))
(define key-state
  (lambda (rt keycode minval maxval lag)
    (mk-ugen (list "KeyState" rt (list keycode minval maxval lag) nil 1 nil nil))))
(define key-track
  (lambda (rt chain keydecay chromaleak)
    (mk-ugen (list "KeyTrack" rt (list chain keydecay chromaleak) nil 1 nil nil))))
(define klang
  (lambda (rt specificationsArrayRef freqscale freqoffset)
    (mk-ugen (list "Klang" rt (list specificationsArrayRef freqscale) freqoffset 1 nil nil))))
(define klank
  (lambda (specificationsArrayRef input freqscale freqoffset decayscale)
    (mk-ugen (list "Klank" (list 0) (list specificationsArrayRef input freqscale freqoffset) decayscale 1 nil nil))))
(define lf-clip-noise
  (lambda (rt freq)
    (mk-ugen (list "LFClipNoise" rt (list freq) nil 1 nil (incr-uid 1)))))
(define lf-cub
  (lambda (rt freq iphase)
    (mk-ugen (list "LFCub" rt (list freq iphase) nil 1 nil nil))))
(define lfd-clip-noise
  (lambda (rt freq)
    (mk-ugen (list "LFDClipNoise" rt (list freq) nil 1 nil (incr-uid 1)))))
(define lfd-noise0
  (lambda (rt freq)
    (mk-ugen (list "LFDNoise0" rt (list freq) nil 1 nil (incr-uid 1)))))
(define lfd-noise1
  (lambda (rt freq)
    (mk-ugen (list "LFDNoise1" rt (list freq) nil 1 nil (incr-uid 1)))))
(define lfd-noise3
  (lambda (rt freq)
    (mk-ugen (list "LFDNoise3" rt (list freq) nil 1 nil (incr-uid 1)))))
(define lfgauss
  (lambda (rt duration width iphase loop doneAction)
    (mk-ugen (list "LFGauss" rt (list duration width iphase loop doneAction) nil 1 nil nil))))
(define lf-noise0
  (lambda (rt freq)
    (mk-ugen (list "LFNoise0" rt (list freq) nil 1 nil (incr-uid 1)))))
(define lf-noise1
  (lambda (rt freq)
    (mk-ugen (list "LFNoise1" rt (list freq) nil 1 nil (incr-uid 1)))))
(define lf-noise2
  (lambda (rt freq)
    (mk-ugen (list "LFNoise2" rt (list freq) nil 1 nil (incr-uid 1)))))
(define lf-par
  (lambda (rt freq iphase)
    (mk-ugen (list "LFPar" rt (list freq iphase) nil 1 nil nil))))
(define lf-pulse
  (lambda (rt freq iphase width)
    (mk-ugen (list "LFPulse" rt (list freq iphase width) nil 1 nil nil))))
(define lf-saw
  (lambda (rt freq iphase)
    (mk-ugen (list "LFSaw" rt (list freq iphase) nil 1 nil nil))))
(define lf-tri
  (lambda (rt freq iphase)
    (mk-ugen (list "LFTri" rt (list freq iphase) nil 1 nil nil))))
(define lpf
  (lambda (in freq)
    (mk-ugen (list "LPF" (list 0) (list in freq) nil 1 nil nil))))
(define lpz1
  (lambda (in)
    (mk-ugen (list "LPZ1" (list 0) (list in) nil 1 nil nil))))
(define lpz2
  (lambda (in)
    (mk-ugen (list "LPZ2" (list 0) (list in) nil 1 nil nil))))
(define lag
  (lambda (in lagTime)
    (mk-ugen (list "Lag" (list 0) (list in lagTime) nil 1 nil nil))))
(define lag2
  (lambda (in lagTime)
    (mk-ugen (list "Lag2" (list 0) (list in lagTime) nil 1 nil nil))))
(define lag2ud
  (lambda (in lagTimeU lagTimeD)
    (mk-ugen (list "Lag2UD" (list 0) (list in lagTimeU lagTimeD) nil 1 nil nil))))
(define lag3
  (lambda (in lagTime)
    (mk-ugen (list "Lag3" (list 0) (list in lagTime) nil 1 nil nil))))
(define lag3ud
  (lambda (in lagTimeU lagTimeD)
    (mk-ugen (list "Lag3UD" (list 0) (list in lagTimeU lagTimeD) nil 1 nil nil))))
(define lag-control
  (lambda (rt values lags)
    (mk-ugen (list "LagControl" rt (list values lags) nil 1 nil nil))))
(define lag-in
  (lambda (nc rt bus lag)
    (mk-ugen (list "LagIn" rt (list bus lag) nil nc nil nil))))
(define lag-ud
  (lambda (in lagTimeU lagTimeD)
    (mk-ugen (list "LagUD" (list 0) (list in lagTimeU lagTimeD) nil 1 nil nil))))
(define last-value
  (lambda (in diff)
    (mk-ugen (list "LastValue" (list 0) (list in diff) nil 1 nil nil))))
(define latch
  (lambda (in trig)
    (mk-ugen (list "Latch" (list 0) (list in trig) nil 1 nil nil))))
(define latoocarfian-c
  (lambda (rt freq a b c d xi yi)
    (mk-ugen (list "LatoocarfianC" rt (list freq a b c d xi yi) nil 1 nil nil))))
(define latoocarfian-l
  (lambda (rt freq a b c d xi yi)
    (mk-ugen (list "LatoocarfianL" rt (list freq a b c d xi yi) nil 1 nil nil))))
(define latoocarfian-n
  (lambda (rt freq a b c d xi yi)
    (mk-ugen (list "LatoocarfianN" rt (list freq a b c d xi yi) nil 1 nil nil))))
(define leak-dc
  (lambda (in coef)
    (mk-ugen (list "LeakDC" (list 0) (list in coef) nil 1 nil nil))))
(define least-change
  (lambda (rt a b)
    (mk-ugen (list "LeastChange" rt (list a b) nil 1 nil nil))))
(define limiter
  (lambda (in level dur)
    (mk-ugen (list "Limiter" (list 0) (list in level dur) nil 1 nil nil))))
(define lin-cong-c
  (lambda (rt freq a c m xi)
    (mk-ugen (list "LinCongC" rt (list freq a c m xi) nil 1 nil nil))))
(define lin-cong-l
  (lambda (rt freq a c m xi)
    (mk-ugen (list "LinCongL" rt (list freq a c m xi) nil 1 nil nil))))
(define lin-cong-n
  (lambda (rt freq a c m xi)
    (mk-ugen (list "LinCongN" rt (list freq a c m xi) nil 1 nil nil))))
(define lin-exp
  (lambda (in srclo srchi dstlo dsthi)
    (mk-ugen (list "LinExp" (list 0) (list in srclo srchi dstlo dsthi) nil 1 nil nil))))
(define lin-pan2
  (lambda (in pos level)
    (mk-ugen (list "LinPan2" (list 0) (list in pos level) nil 2 nil nil))))
(define lin-rand
  (lambda (lo hi minmax)
    (mk-ugen (list "LinRand" (list 0 1) (list lo hi minmax) nil 1 nil (incr-uid 1)))))
(define lin-x-fade2
  (lambda (rt inA inB pan level)
    (mk-ugen (list "LinXFade2" rt (list inA inB pan level) nil 1 nil nil))))
(define line
  (lambda (rt start end dur doneAction)
    (mk-ugen (list "Line" rt (list start end dur doneAction) nil 1 nil nil))))
(define linen
  (lambda (rt gate attackTime susLevel releaseTime doneAction)
    (mk-ugen (list "Linen" rt (list gate attackTime susLevel releaseTime doneAction) nil 1 nil nil))))
(define list-dugen
  (lambda (list_ repeats)
    (mk-ugen (list "ListDUGen" dr (list list_ repeats) nil 1 nil nil))))
(define local-buf
  (lambda (numChannels numFrames)
    (mk-ugen (list "LocalBuf" ir (list numChannels numFrames) nil 1 nil (incr-uid 1)))))
(define local-in
  (lambda (nc rt default)
    (mk-ugen (list "LocalIn" rt nil default nc nil nil))))
(define local-out
  (lambda (channelsArray)
    (mk-ugen (list "LocalOut" (list 0) nil channelsArray 1 nil nil))))
(define logistic
  (lambda (rt chaosParam freq init)
    (mk-ugen (list "Logistic" rt (list chaosParam freq init) nil 1 nil nil))))
(define lorenz-l
  (lambda (rt freq s r b h xi yi zi)
    (mk-ugen (list "LorenzL" rt (list freq s r b h xi yi zi) nil 1 nil nil))))
(define loudness
  (lambda (rt chain smask tmask)
    (mk-ugen (list "Loudness" rt (list chain smask tmask) nil 1 nil nil))))
(define mfcc
  (lambda (rt chain numcoeff)
    (mk-ugen (list "MFCC" rt (list chain numcoeff) nil 13 nil nil))))
(define mantissa-mask
  (lambda (in bits)
    (mk-ugen (list "MantissaMask" (list 0) (list in bits) nil 1 nil nil))))
(define max-local-bufs
  (lambda (rt)
    (mk-ugen (list "MaxLocalBufs" rt nil nil 1 nil nil))))
(define median
  (lambda (length in)
    (mk-ugen (list "Median" (list 1) (list length in) nil 1 nil nil))))
(define mid-eq
  (lambda (in freq rq db)
    (mk-ugen (list "MidEQ" (list 0) (list in freq rq db) nil 1 nil nil))))
(define mod-dif
  (lambda (rt x y mod)
    (mk-ugen (list "ModDif" rt (list x y mod) nil 1 nil nil))))
(define moog-ff
  (lambda (in freq gain reset)
    (mk-ugen (list "MoogFF" (list 0) (list in freq gain reset) nil 1 nil nil))))
(define most-change
  (lambda (a b)
    (mk-ugen (list "MostChange" (list 0 1) (list a b) nil 1 nil nil))))
(define mouse-button
  (lambda (rt minval maxval lag)
    (mk-ugen (list "MouseButton" rt (list minval maxval lag) nil 1 nil nil))))
(define mouse-x
  (lambda (rt minval maxval warp lag)
    (mk-ugen (list "MouseX" rt (list minval maxval warp lag) nil 1 nil nil))))
(define mouse-y
  (lambda (rt minval maxval warp lag)
    (mk-ugen (list "MouseY" rt (list minval maxval warp lag) nil 1 nil nil))))
(define n-rand
  (lambda (rt lo hi n)
    (mk-ugen (list "NRand" rt (list lo hi n) nil 1 nil (incr-uid 1)))))
(define normalizer
  (lambda (in level dur)
    (mk-ugen (list "Normalizer" (list 0) (list in level dur) nil 1 nil nil))))
(define num-audio-buses
  (lambda (rt)
    (mk-ugen (list "NumAudioBuses" rt nil nil 1 nil nil))))
(define num-buffers
  (lambda (rt)
    (mk-ugen (list "NumBuffers" rt nil nil 1 nil nil))))
(define num-control-buses
  (lambda (rt)
    (mk-ugen (list "NumControlBuses" rt nil nil 1 nil nil))))
(define num-input-buses
  (lambda (rt)
    (mk-ugen (list "NumInputBuses" rt nil nil 1 nil nil))))
(define num-output-buses
  (lambda (rt)
    (mk-ugen (list "NumOutputBuses" rt nil nil 1 nil nil))))
(define num-running-synths
  (lambda (rt)
    (mk-ugen (list "NumRunningSynths" rt nil nil 1 nil nil))))
(define offset-out
  (lambda (rt bus channelsArray)
    (mk-ugen (list "OffsetOut" rt (list bus) channelsArray 1 nil nil))))
(define one-pole
  (lambda (in coef)
    (mk-ugen (list "OnePole" (list 0) (list in coef) nil 1 nil nil))))
(define one-zero
  (lambda (in coef)
    (mk-ugen (list "OneZero" (list 0) (list in coef) nil 1 nil nil))))
(define onsets
  (lambda (rt chain threshold odftype relaxtime floor mingap medianspan whtype rawodf)
    (mk-ugen (list "Onsets" rt (list chain threshold odftype relaxtime floor mingap medianspan whtype rawodf) nil 1 nil nil))))
(define osc
  (lambda (rt bufnum freq phase)
    (mk-ugen (list "Osc" rt (list bufnum freq phase) nil 1 nil nil))))
(define osc-n
  (lambda (rt bufnum freq phase)
    (mk-ugen (list "OscN" rt (list bufnum freq phase) nil 1 nil nil))))
(define out
  (lambda (bus channelsArray)
    (mk-ugen (list "Out" (list 1) (list bus) channelsArray 1 nil nil))))
(define p-sin-grain
  (lambda (rt freq dur amp)
    (mk-ugen (list "PSinGrain" rt (list freq dur amp) nil 1 nil nil))))
(define pv-add
  (lambda (rt bufferA bufferB)
    (mk-ugen (list "PV_Add" rt (list bufferA bufferB) nil 1 nil nil))))
(define pv-bin-scramble
  (lambda (rt buffer wipe width trig)
    (mk-ugen (list "PV_BinScramble" rt (list buffer wipe width trig) nil 1 nil nil))))
(define pv-bin-shift
  (lambda (rt buffer stretch shift interp)
    (mk-ugen (list "PV_BinShift" rt (list buffer stretch shift interp) nil 1 nil nil))))
(define pv-bin-wipe
  (lambda (rt bufferA bufferB wipe)
    (mk-ugen (list "PV_BinWipe" rt (list bufferA bufferB wipe) nil 1 nil nil))))
(define pv-brick-wall
  (lambda (rt buffer wipe)
    (mk-ugen (list "PV_BrickWall" rt (list buffer wipe) nil 1 nil nil))))
(define pv-chain-ugen
  (lambda (rt maxSize)
    (mk-ugen (list "PV_ChainUGen" rt (list maxSize) nil 1 nil nil))))
(define pv-conformal-map
  (lambda (rt buffer areal aimag)
    (mk-ugen (list "PV_ConformalMap" rt (list buffer areal aimag) nil 1 nil nil))))
(define pv-conj
  (lambda (rt buffer)
    (mk-ugen (list "PV_Conj" rt (list buffer) nil 1 nil nil))))
(define pv-copy
  (lambda (rt bufferA bufferB)
    (mk-ugen (list "PV_Copy" rt (list bufferA bufferB) nil 1 nil nil))))
(define pv-copy-phase
  (lambda (rt bufferA bufferB)
    (mk-ugen (list "PV_CopyPhase" rt (list bufferA bufferB) nil 1 nil nil))))
(define pv-diffuser
  (lambda (rt buffer trig)
    (mk-ugen (list "PV_Diffuser" rt (list buffer trig) nil 1 nil nil))))
(define pv-div
  (lambda (rt bufferA bufferB)
    (mk-ugen (list "PV_Div" rt (list bufferA bufferB) nil 1 nil nil))))
(define pv-hainsworth-foote
  (lambda (rt maxSize)
    (mk-ugen (list "PV_HainsworthFoote" rt (list maxSize) nil 1 nil nil))))
(define pv-jensen-andersen
  (lambda (rt maxSize)
    (mk-ugen (list "PV_JensenAndersen" rt (list maxSize) nil 1 nil nil))))
(define pv-local-max
  (lambda (rt buffer threshold)
    (mk-ugen (list "PV_LocalMax" rt (list buffer threshold) nil 1 nil nil))))
(define pv-mag-above
  (lambda (rt buffer threshold)
    (mk-ugen (list "PV_MagAbove" rt (list buffer threshold) nil 1 nil nil))))
(define pv-mag-below
  (lambda (rt buffer threshold)
    (mk-ugen (list "PV_MagBelow" rt (list buffer threshold) nil 1 nil nil))))
(define pv-mag-clip
  (lambda (rt buffer threshold)
    (mk-ugen (list "PV_MagClip" rt (list buffer threshold) nil 1 nil nil))))
(define pv-mag-div
  (lambda (rt bufferA bufferB zeroed)
    (mk-ugen (list "PV_MagDiv" rt (list bufferA bufferB zeroed) nil 1 nil nil))))
(define pv-mag-freeze
  (lambda (rt buffer freeze)
    (mk-ugen (list "PV_MagFreeze" rt (list buffer freeze) nil 1 nil nil))))
(define pv-mag-mul
  (lambda (rt bufferA bufferB)
    (mk-ugen (list "PV_MagMul" rt (list bufferA bufferB) nil 1 nil nil))))
(define pv-mag-noise
  (lambda (rt buffer)
    (mk-ugen (list "PV_MagNoise" rt (list buffer) nil 1 nil nil))))
(define pv-mag-shift
  (lambda (rt buffer stretch shift)
    (mk-ugen (list "PV_MagShift" rt (list buffer stretch shift) nil 1 nil nil))))
(define pv-mag-smear
  (lambda (rt buffer bins)
    (mk-ugen (list "PV_MagSmear" rt (list buffer bins) nil 1 nil nil))))
(define pv-mag-squared
  (lambda (rt buffer)
    (mk-ugen (list "PV_MagSquared" rt (list buffer) nil 1 nil nil))))
(define pv-max
  (lambda (rt bufferA bufferB)
    (mk-ugen (list "PV_Max" rt (list bufferA bufferB) nil 1 nil nil))))
(define pv-min
  (lambda (rt bufferA bufferB)
    (mk-ugen (list "PV_Min" rt (list bufferA bufferB) nil 1 nil nil))))
(define pv-mul
  (lambda (rt bufferA bufferB)
    (mk-ugen (list "PV_Mul" rt (list bufferA bufferB) nil 1 nil nil))))
(define pv-phase-shift
  (lambda (rt buffer shift integrate)
    (mk-ugen (list "PV_PhaseShift" rt (list buffer shift integrate) nil 1 nil nil))))
(define pv-phase-shift270
  (lambda (rt buffer)
    (mk-ugen (list "PV_PhaseShift270" rt (list buffer) nil 1 nil nil))))
(define pv-phase-shift90
  (lambda (rt buffer)
    (mk-ugen (list "PV_PhaseShift90" rt (list buffer) nil 1 nil nil))))
(define pv-rand-comb
  (lambda (rt buffer wipe trig)
    (mk-ugen (list "PV_RandComb" rt (list buffer wipe trig) nil 1 nil nil))))
(define pv-rand-wipe
  (lambda (rt bufferA bufferB wipe trig)
    (mk-ugen (list "PV_RandWipe" rt (list bufferA bufferB wipe trig) nil 1 nil nil))))
(define pv-rect-comb
  (lambda (rt buffer numTeeth phase width)
    (mk-ugen (list "PV_RectComb" rt (list buffer numTeeth phase width) nil 1 nil nil))))
(define pv-rect-comb2
  (lambda (rt bufferA bufferB numTeeth phase width)
    (mk-ugen (list "PV_RectComb2" rt (list bufferA bufferB numTeeth phase width) nil 1 nil nil))))
(define pv-split
  (lambda (rt bufferA bufferB)
    (mk-ugen (list "PV_Split" rt (list bufferA bufferB) nil 2 nil nil))))
(define pan2
  (lambda (in pos level)
    (mk-ugen (list "Pan2" (list 0) (list in pos level) nil 2 nil nil))))
(define pan4
  (lambda (rt in xpos ypos level)
    (mk-ugen (list "Pan4" rt (list in xpos ypos level) nil 4 nil nil))))
(define pan-az
  (lambda (nc rt in pos level width orientation)
    (mk-ugen (list "PanAz" rt (list in pos level width orientation) nil nc nil nil))))
(define pan-b
  (lambda (rt in azimuth elevation gain)
    (mk-ugen (list "PanB" rt (list in azimuth elevation gain) nil 4 nil nil))))
(define pan-b2
  (lambda (rt in azimuth gain)
    (mk-ugen (list "PanB2" rt (list in azimuth gain) nil 3 nil nil))))
(define part-conv
  (lambda (rt in fftsize irbufnum)
    (mk-ugen (list "PartConv" rt (list in fftsize irbufnum) nil 1 nil nil))))
(define pause
  (lambda (rt gate id)
    (mk-ugen (list "Pause" rt (list gate id) nil 1 nil nil))))
(define pause-self
  (lambda (rt in)
    (mk-ugen (list "PauseSelf" rt (list in) nil 1 nil nil))))
(define pause-self-when-done
  (lambda (rt src)
    (mk-ugen (list "PauseSelfWhenDone" rt (list src) nil 1 nil nil))))
(define peak
  (lambda (in trig)
    (mk-ugen (list "Peak" (list 0) (list in trig) nil 1 nil nil))))
(define peak-follower
  (lambda (in decay)
    (mk-ugen (list "PeakFollower" (list 0) (list in decay) nil 1 nil nil))))
(define phasor
  (lambda (rt trig rate start end resetPos)
    (mk-ugen (list "Phasor" rt (list trig rate start end resetPos) nil 1 nil nil))))
(define pink-noise
  (lambda (rt)
    (mk-ugen (list "PinkNoise" rt nil nil 1 nil (incr-uid 1)))))
(define pitch
  (lambda (rt in initFreq minFreq maxFreq execFreq maxBinsPerOctave median ampThreshold peakThreshold downSample clar)
    (mk-ugen (list "Pitch" rt (list in initFreq minFreq maxFreq execFreq maxBinsPerOctave median ampThreshold peakThreshold downSample clar) nil 2 nil nil))))
(define pitch-shift
  (lambda (in windowSize pitchRatio pitchDispersion timeDispersion)
    (mk-ugen (list "PitchShift" (list 0) (list in windowSize pitchRatio pitchDispersion timeDispersion) nil 1 nil nil))))
(define play-buf
  (lambda (rt bufnum rate trigger startPos loop doneAction)
    (mk-ugen (list "PlayBuf" rt (list bufnum rate trigger startPos loop doneAction) nil 1 nil nil))))
(define pluck
  (lambda (in trig maxdelaytime delaytime decaytime coef)
    (mk-ugen (list "Pluck" (list 0) (list in trig maxdelaytime delaytime decaytime coef) nil 1 nil nil))))
(define poll
  (lambda (rt trig in label trigid)
    (mk-ugen (list "Poll" rt (list trig in label trigid) nil 1 nil nil))))
(define pulse
  (lambda (rt freq width)
    (mk-ugen (list "Pulse" rt (list freq width) nil 1 nil nil))))
(define pulse-count
  (lambda (trig reset)
    (mk-ugen (list "PulseCount" (list 0) (list trig reset) nil 1 nil nil))))
(define pulse-divider
  (lambda (trig div start)
    (mk-ugen (list "PulseDivider" (list 0) (list trig div start) nil 1 nil nil))))
(define pure-multi-out-ugen
  (lambda (rt maxSize)
    (mk-ugen (list "PureMultiOutUGen" rt (list maxSize) nil 1 nil nil))))
(define pure-ugen
  (lambda (rt maxSize)
    (mk-ugen (list "PureUGen" rt (list maxSize) nil 1 nil nil))))
(define quad-c
  (lambda (rt freq a b c xi)
    (mk-ugen (list "QuadC" rt (list freq a b c xi) nil 1 nil nil))))
(define quad-l
  (lambda (rt freq a b c xi)
    (mk-ugen (list "QuadL" rt (list freq a b c xi) nil 1 nil nil))))
(define quad-n
  (lambda (rt freq a b c xi)
    (mk-ugen (list "QuadN" rt (list freq a b c xi) nil 1 nil nil))))
(define r-delay-map
  (lambda (rt bufnum in dynamic spec)
    (mk-ugen (list "RDelayMap" rt (list bufnum in dynamic spec) nil 1 nil nil))))
(define r-delay-set
  (lambda (rt in spec)
    (mk-ugen (list "RDelaySet" rt (list in spec) nil 1 nil nil))))
(define r-delay-set-b
  (lambda (rt bufnum in spec)
    (mk-ugen (list "RDelaySetB" rt (list bufnum in spec) nil 1 nil nil))))
(define r-freezer
  (lambda (rt bufnum left right gain increment incrementOffset incrementRandom rightRandom syncPhaseTrigger randomizePhaseTrigger numberOfLoops)
    (mk-ugen (list "RFreezer" rt (list bufnum left right gain increment incrementOffset incrementRandom rightRandom syncPhaseTrigger randomizePhaseTrigger numberOfLoops) nil 1 nil nil))))
(define rhpf
  (lambda (in freq rq)
    (mk-ugen (list "RHPF" (list 0) (list in freq rq) nil 1 nil nil))))
(define rlpf
  (lambda (in freq rq)
    (mk-ugen (list "RLPF" (list 0) (list in freq rq) nil 1 nil nil))))
(define r-loop-set
  (lambda (rt bufnum left right gain increment spec)
    (mk-ugen (list "RLoopSet" rt (list bufnum left right gain increment spec) nil 1 nil nil))))
(define r-play-trace
  (lambda (rt bufnum degree rate axis)
    (mk-ugen (list "RPlayTrace" rt (list bufnum degree rate axis) nil 1 nil nil))))
(define r-shuffler-b
  (lambda (rt bufnum readLocationMinima readLocationMaxima readIncrementMinima readIncrementMaxima durationMinima durationMaxima envelopeAmplitudeMinima envelopeAmplitudeMaxima envelopeShapeMinima envelopeShapeMaxima envelopeSkewMinima envelopeSkewMaxima stereoLocationMinima stereoLocationMaxima interOffsetTimeMinima interOffsetTimeMaxima ftableReadLocationIncrement readIncrementQuanta interOffsetTimeQuanta)
    (mk-ugen (list "RShufflerB" rt (list bufnum readLocationMinima readLocationMaxima readIncrementMinima readIncrementMaxima durationMinima durationMaxima envelopeAmplitudeMinima envelopeAmplitudeMaxima envelopeShapeMinima envelopeShapeMaxima envelopeSkewMinima envelopeSkewMaxima stereoLocationMinima stereoLocationMaxima interOffsetTimeMinima interOffsetTimeMaxima ftableReadLocationIncrement readIncrementQuanta interOffsetTimeQuanta) nil 2 nil nil))))
(define r-shuffler-l
  (lambda (rt in fragmentSize maxDelay)
    (mk-ugen (list "RShufflerL" rt (list in fragmentSize maxDelay) nil 1 nil nil))))
(define r-trace-rd
  (lambda (rt bufnum degree index axis)
    (mk-ugen (list "RTraceRd" rt (list bufnum degree index axis) nil 1 nil nil))))
(define r-trace-rd-x
  (lambda (rt bufnum degree index)
    (mk-ugen (list "RTraceRdX" rt (list bufnum degree index) nil 1 nil nil))))
(define r-trace-rd-y
  (lambda (rt bufnum degree index)
    (mk-ugen (list "RTraceRdY" rt (list bufnum degree index) nil 1 nil nil))))
(define r-trace-rd-z
  (lambda (rt bufnum degree index)
    (mk-ugen (list "RTraceRdZ" rt (list bufnum degree index) nil 1 nil nil))))
(define radians-per-sample
  (lambda (rt)
    (mk-ugen (list "RadiansPerSample" rt nil nil 1 nil nil))))
(define ramp
  (lambda (in lagTime)
    (mk-ugen (list "Ramp" (list 0) (list in lagTime) nil 1 nil nil))))
(define rand
  (lambda (lo hi)
    (mk-ugen (list "Rand" (list 0 1) (list lo hi) nil 1 nil (incr-uid 1)))))
(define rand-id
  (lambda (rt id)
    (mk-ugen (list "RandID" rt (list id) nil 1 nil nil))))
(define rand-seed
  (lambda (rt trig seed)
    (mk-ugen (list "RandSeed" rt (list trig seed) nil 1 nil nil))))
(define record-buf
  (lambda (rt inputArray bufnum offset recLevel preLevel run loop trigger doneAction)
    (mk-ugen (list "RecordBuf" rt (list inputArray bufnum offset recLevel preLevel run loop trigger) doneAction 1 nil nil))))
(define replace-out
  (lambda (bus channelsArray)
    (mk-ugen (list "ReplaceOut" (list 1) (list bus) channelsArray 1 nil nil))))
(define resonz
  (lambda (in freq bwr)
    (mk-ugen (list "Resonz" (list 0) (list in freq bwr) nil 1 nil nil))))
(define ringz
  (lambda (in freq decaytime)
    (mk-ugen (list "Ringz" (list 0) (list in freq decaytime) nil 1 nil nil))))
(define rotate2
  (lambda (x y pos)
    (mk-ugen (list "Rotate2" (list 0 1) (list x y pos) nil 2 nil nil))))
(define running-max
  (lambda (in trig)
    (mk-ugen (list "RunningMax" (list 0) (list in trig) nil 1 nil nil))))
(define running-min
  (lambda (in trig)
    (mk-ugen (list "RunningMin" (list 0) (list in trig) nil 1 nil nil))))
(define running-sum
  (lambda (in numsamp)
    (mk-ugen (list "RunningSum" (list 0) (list in numsamp) nil 1 nil nil))))
(define sos
  (lambda (in a0 a1 a2 b1 b2)
    (mk-ugen (list "SOS" (list 0) (list in a0 a1 a2 b1 b2) nil 1 nil nil))))
(define sample-dur (mk-ugen (list "SampleDur" ir nil nil 1 nil nil)))
(define sample-rate (mk-ugen (list "SampleRate" ir nil nil 1 nil nil)))
(define saw
  (lambda (rt freq)
    (mk-ugen (list "Saw" rt (list freq) nil 1 nil nil))))
(define schmidt
  (lambda (rt in lo hi)
    (mk-ugen (list "Schmidt" rt (list in lo hi) nil 1 nil nil))))
(define scope-out
  (lambda (rt inputArray bufnum)
    (mk-ugen (list "ScopeOut" rt (list inputArray bufnum) nil 1 nil nil))))
(define scope-out2
  (lambda (rt inputArray scopeNum maxFrames scopeFrames)
    (mk-ugen (list "ScopeOut2" rt (list inputArray scopeNum maxFrames scopeFrames) nil 1 nil nil))))
(define select
  (lambda (which array)
    (mk-ugen (list "Select" (list 0 1) (list which) array 1 nil nil))))
(define send-trig
  (lambda (in id value)
    (mk-ugen (list "SendTrig" (list 0) (list in id value) nil 1 nil nil))))
(define set-buf
  (lambda (buf numValues offset values)
    (mk-ugen (list "SetBuf" ir (list buf numValues offset) values 1 nil nil))))
(define set-reset-ff
  (lambda (trig reset)
    (mk-ugen (list "SetResetFF" (list 0) (list trig reset) nil 1 nil nil))))
(define shaper
  (lambda (bufnum in)
    (mk-ugen (list "Shaper" (list 1) (list bufnum in) nil 1 nil nil))))
(define sin-osc
  (lambda (rt freq phase)
    (mk-ugen (list "SinOsc" rt (list freq phase) nil 1 nil nil))))
(define sin-osc-fb
  (lambda (rt freq feedback)
    (mk-ugen (list "SinOscFB" rt (list freq feedback) nil 1 nil nil))))
(define slew
  (lambda (in up dn)
    (mk-ugen (list "Slew" (list 0) (list in up dn) nil 1 nil nil))))
(define slope
  (lambda (in)
    (mk-ugen (list "Slope" (list 0) (list in) nil 1 nil nil))))
(define spec-centroid
  (lambda (rt buffer)
    (mk-ugen (list "SpecCentroid" rt (list buffer) nil 1 nil nil))))
(define spec-flatness
  (lambda (rt buffer)
    (mk-ugen (list "SpecFlatness" rt (list buffer) nil 1 nil nil))))
(define spec-pcile
  (lambda (rt buffer fraction interpolate)
    (mk-ugen (list "SpecPcile" rt (list buffer fraction interpolate) nil 1 nil nil))))
(define splay
  (lambda (rt inArray spread level center levelComp)
    (mk-ugen (list "Splay" rt (list inArray spread level center levelComp) nil 2 nil nil))))
(define splay-az
  (lambda (rt inArray spread level width center orientation levelComp)
    (mk-ugen (list "SplayAz" rt (list inArray spread level width center orientation levelComp) nil 1 nil nil))))
(define spring
  (lambda (rt in spring damp)
    (mk-ugen (list "Spring" rt (list in spring damp) nil 1 nil nil))))
(define standard-l
  (lambda (rt freq k xi yi)
    (mk-ugen (list "StandardL" rt (list freq k xi yi) nil 1 nil nil))))
(define standard-n
  (lambda (rt freq k xi yi)
    (mk-ugen (list "StandardN" rt (list freq k xi yi) nil 1 nil nil))))
(define stepper
  (lambda (trig reset min max step resetval)
    (mk-ugen (list "Stepper" (list 0) (list trig reset min max step resetval) nil 1 nil nil))))
(define stereo-convolution2l
  (lambda (rt in kernelL kernelR trigger framesize crossfade)
    (mk-ugen (list "StereoConvolution2L" rt (list in kernelL kernelR trigger framesize crossfade) nil 2 nil nil))))
(define subsample-offset
  (lambda (rt)
    (mk-ugen (list "SubsampleOffset" rt nil nil 1 nil nil))))
(define sum3
  (lambda (in0 in1 in2)
    (mk-ugen (list "Sum3" (list 0 1 2) (list in0 in1 in2) nil 1 nil nil))))
(define sum4
  (lambda (in0 in1 in2 in3)
    (mk-ugen (list "Sum4" (list 0 1 2 3) (list in0 in1 in2 in3) nil 1 nil nil))))
(define sweep
  (lambda (trig rate)
    (mk-ugen (list "Sweep" (list 0) (list trig rate) nil 1 nil nil))))
(define sync-saw
  (lambda (rt syncFreq sawFreq)
    (mk-ugen (list "SyncSaw" rt (list syncFreq sawFreq) nil 1 nil nil))))
(define t2a
  (lambda (rt in offset)
    (mk-ugen (list "T2A" rt (list in offset) nil 1 nil nil))))
(define t2k
  (lambda (rt in)
    (mk-ugen (list "T2K" rt (list in) nil 1 nil nil))))
(define t-ball
  (lambda (rt in g damp friction)
    (mk-ugen (list "TBall" rt (list in g damp friction) nil 1 nil nil))))
(define t-delay
  (lambda (in dur)
    (mk-ugen (list "TDelay" (list 0) (list in dur) nil 1 nil nil))))
(define t-duty
  (lambda (rt dur reset level doneAction gapFirst)
    (mk-ugen (list "TDuty" rt (list dur reset level doneAction gapFirst) nil 1 nil nil))))
(define t-exp-rand
  (lambda (lo hi trig)
    (mk-ugen (list "TExpRand" (list 2) (list lo hi trig) nil 1 nil (incr-uid 1)))))
(define tgrains
  (lambda (nc rt trigger bufnum rate centerPos dur pan amp interp)
    (mk-ugen (list "TGrains" rt (list trigger bufnum rate centerPos dur pan amp interp) nil nc nil nil))))
(define ti-rand
  (lambda (lo hi trig)
    (mk-ugen (list "TIRand" (list 2) (list lo hi trig) nil 1 nil (incr-uid 1)))))
(define t-rand
  (lambda (lo hi trig)
    (mk-ugen (list "TRand" (list 2) (list lo hi trig) nil 1 nil (incr-uid 1)))))
(define t-windex
  (lambda (in array normalize)
    (mk-ugen (list "TWindex" (list 0) (list in array) normalize 1 nil (incr-uid 1)))))
(define tap
  (lambda (nc rt bufnum delaytime)
    (mk-ugen (list "Tap" rt (list bufnum delaytime) nil nc nil nil))))
(define timer
  (lambda (trig)
    (mk-ugen (list "Timer" (list 0) (list trig) nil 1 nil nil))))
(define toggle-ff
  (lambda (trig)
    (mk-ugen (list "ToggleFF" (list 0) (list trig) nil 1 nil nil))))
(define trig
  (lambda (in dur)
    (mk-ugen (list "Trig" (list 0) (list in dur) nil 1 nil nil))))
(define trig1
  (lambda (in dur)
    (mk-ugen (list "Trig1" (list 0) (list in dur) nil 1 nil nil))))
(define trig-control
  (lambda (rt values)
    (mk-ugen (list "TrigControl" rt (list values) nil 1 nil nil))))
(define two-pole
  (lambda (in freq radius)
    (mk-ugen (list "TwoPole" (list 0) (list in freq radius) nil 1 nil nil))))
(define two-zero
  (lambda (in freq radius)
    (mk-ugen (list "TwoZero" (list 0) (list in freq radius) nil 1 nil nil))))
(define unary-op-ugen
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 nil nil))))
(define v-disk-in
  (lambda (nc rt bufnum rate loop sendID)
    (mk-ugen (list "VDiskIn" rt (list bufnum rate loop sendID) nil nc nil nil))))
(define v-osc
  (lambda (rt bufpos freq phase)
    (mk-ugen (list "VOsc" rt (list bufpos freq phase) nil 1 nil nil))))
(define v-osc3
  (lambda (rt bufpos freq1 freq2 freq3)
    (mk-ugen (list "VOsc3" rt (list bufpos freq1 freq2 freq3) nil 1 nil nil))))
(define var-lag
  (lambda (rt in time curvature warp start)
    (mk-ugen (list "VarLag" rt (list in time curvature warp start) nil 1 nil nil))))
(define var-saw
  (lambda (rt freq iphase width)
    (mk-ugen (list "VarSaw" rt (list freq iphase width) nil 1 nil nil))))
(define vibrato
  (lambda (rt freq rate depth delay onset rateVariation depthVariation iphase)
    (mk-ugen (list "Vibrato" rt (list freq rate depth delay onset rateVariation depthVariation iphase) nil 1 nil (incr-uid 1)))))
(define warp1
  (lambda (nc bufnum pointer freqScale windowSize envbufnum overlaps windowRandRatio interp)
    (mk-ugen (list "Warp1" ar (list bufnum pointer freqScale windowSize envbufnum overlaps windowRandRatio interp) nil nc nil nil))))
(define white-noise
  (lambda (rt)
    (mk-ugen (list "WhiteNoise" rt nil nil 1 nil (incr-uid 1)))))
(define width-first-ugen
  (lambda (rt maxSize)
    (mk-ugen (list "WidthFirstUGen" rt (list maxSize) nil 1 nil nil))))
(define wrap
  (lambda (in lo hi)
    (mk-ugen (list "Wrap" (list 0) (list in lo hi) nil 1 nil nil))))
(define wrap-index
  (lambda (bufnum in)
    (mk-ugen (list "WrapIndex" (list 1) (list bufnum in) nil 1 nil nil))))
(define x-fade2
  (lambda (rt inA inB pan level)
    (mk-ugen (list "XFade2" rt (list inA inB pan level) nil 1 nil nil))))
(define x-line
  (lambda (rt start end dur doneAction)
    (mk-ugen (list "XLine" rt (list start end dur doneAction) nil 1 nil nil))))
(define x-out
  (lambda (bus xfade channelsArray)
    (mk-ugen (list "XOut" (list 2) (list bus xfade) channelsArray 1 nil nil))))
(define zero-crossing
  (lambda (rt in)
    (mk-ugen (list "ZeroCrossing" rt (list in) nil 1 nil nil))))
(define neg
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 0 nil))))
(define not
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 1 nil))))
(define is-nil
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 2 nil))))
(define not-nil
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 3 nil))))
(define bit-not
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 4 nil))))
(define abs
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 5 nil))))
(define as-float
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 6 nil))))
(define as-int
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 7 nil))))
(define ceil
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 8 nil))))
(define floor
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 9 nil))))
(define frac
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 10 nil))))
(define sign
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 11 nil))))
(define squared
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 12 nil))))
(define cubed
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 13 nil))))
(define sqrt
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 14 nil))))
(define exp
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 15 nil))))
(define recip
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 16 nil))))
(define midicps
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 17 nil))))
(define cpsmidi
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 18 nil))))
(define midi-ratio
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 19 nil))))
(define ratio-midi
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 20 nil))))
(define db-amp
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 21 nil))))
(define amp-db
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 22 nil))))
(define oct-cps
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 23 nil))))
(define cps-oct
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 24 nil))))
(define log
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 25 nil))))
(define log2
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 26 nil))))
(define log10
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 27 nil))))
(define sin
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 28 nil))))
(define cos
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 29 nil))))
(define tan
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 30 nil))))
(define arc-sin
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 31 nil))))
(define arc-cos
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 32 nil))))
(define arc-tan
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 33 nil))))
(define sin-h
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 34 nil))))
(define cos-h
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 35 nil))))
(define tan-h
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 36 nil))))
(define rand-
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 37 nil))))
(define rand2
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 38 nil))))
(define lin-rand-
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 39 nil))))
(define bi-lin-rand
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 40 nil))))
(define sum3rand
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 41 nil))))
(define distort
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 42 nil))))
(define soft-clip
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 43 nil))))
(define coin
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 44 nil))))
(define digit-value
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 45 nil))))
(define silence
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 46 nil))))
(define thru
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 47 nil))))
(define rect-window
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 48 nil))))
(define han-window
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 49 nil))))
(define welch-window
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 50 nil))))
(define tri-window
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 51 nil))))
(define ramp-
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 52 nil))))
(define s-curve
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 53 nil))))
(define add
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 0 nil))))
(define sub
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 1 nil))))
(define mul
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 2 nil))))
(define i-div
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 3 nil))))
(define f-div
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 4 nil))))
(define mod
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 5 nil))))
(define eq-
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 6 nil))))
(define ne
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 7 nil))))
(define lt-
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 8 nil))))
(define gt-
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 9 nil))))
(define le
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 10 nil))))
(define ge
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 11 nil))))
(define min
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 12 nil))))
(define max
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 13 nil))))
(define bit-and
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 14 nil))))
(define bit-or
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 15 nil))))
(define bit-xor
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 16 nil))))
(define lcm
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 17 nil))))
(define gcd
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 18 nil))))
(define round
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 19 nil))))
(define round-up
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 20 nil))))
(define trunc
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 21 nil))))
(define atan2
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 22 nil))))
(define hypot
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 23 nil))))
(define hypotx
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 24 nil))))
(define pow
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 25 nil))))
(define shift-left
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 26 nil))))
(define shift-right
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 27 nil))))
(define unsigned-shift
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 28 nil))))
(define fill
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 29 nil))))
(define ring1
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 30 nil))))
(define ring2
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 31 nil))))
(define ring3
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 32 nil))))
(define ring4
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 33 nil))))
(define dif-sqr
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 34 nil))))
(define sum-sqr
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 35 nil))))
(define sqr-sum
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 36 nil))))
(define sqr-dif
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 37 nil))))
(define abs-dif
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 38 nil))))
(define thresh
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 39 nil))))
(define am-clip
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 40 nil))))
(define scale-neg
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 41 nil))))
(define clip2
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 42 nil))))
(define excess
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 43 nil))))
(define fold2
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 44 nil))))
(define wrap2
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 45 nil))))
(define first-arg
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 46 nil))))
(define rand-range
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 47 nil))))
(define exp-rand-range
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 48 nil))))
(define +
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 0 nil))))
(define -
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 1 nil))))
(define *
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 2 nil))))
(define /
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 4 nil))))
(define %
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 5 nil))))
(define ==
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 6 nil))))
(define /=
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 7 nil))))
(define <
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 8 nil))))
(define >
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 9 nil))))
(define <=
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 10 nil))))
(define >=
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 11 nil))))
(define **
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 25 nil))))
