\ sinosc at 440hz ; annotated rate
440 0 sinosc.ar 0.1 *

\ sinosc at 440hz ; implicit rate
440 0 sinosc 0.1 *

\ sinosc at 440hz (left) and 441hz (right)
440 441 2 mce 0 sinosc 0.1 *

\ sinosc pair amplitude modulated by sinosc
440 441 2 mce 0 sinosc 1 0 sinosc.kr 0.1 * abs *
