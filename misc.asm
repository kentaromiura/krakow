; originally posted 2004/04/13 https://www.freeforumzone.com/mobile/d/2925993/LaPasseraSoftware-Presenta-KraKow/discussione.aspx?p=1&pl=6&idm1=37268084
; MIT license
SEGMENT .data
SEED db 200,103,43,23,65,77,12,44,45,22,54,76,203,225,0,6,34,150,104,164
     db 255,175,125,244,83,156,236,143,247,35,133,168,250,32,120,121,205
     db 3,146,194,96,172,37,248,127,28,137,136,217,230,105,160,198,8,1
     db 188,60,213,220,98,174,16,179,87,10,84,232,223,39,252,2,237,241
     db 138,112,48,106,141,195,159,73,38,177,169,114,19,145,123,231,181
     db 253,56,91,92,153,190,117,152,187,222,201,166,131,183,149,128,162
     db 196,24,58,186,74,110,47,82,227,193,132,101,79,245,134,50,11,142
     db 18,207,90,176,81,14,167,71,129,243,80,46,192,97,33,211,157,66,234
     db 100,7,180,135,42,239,108,26,17,210,93,5,144,62,171,209,238,78,182
     db 61,30,170,204,40,215,52,189,64,130,94,235,148,21,89,165,216,15
     db 176,56,9,254,124,68,116,88,102,36,158,184,85,99,119,40,4,229,67
     db 72,206,59,113,13,251,25,139,185,20,122,53,161,70,199,242,41,202
     db 218,57,151,31,249,214,154,155,75,212,191,55,208,51,147,111,63,240
     db 115,27,235,109,228,95,118,69,221,197,140,29,107,163,224,178,246
     db 140,226,219

SEGMENT .text
DELAY:
MOV AX,0XFFFF
DELAY1:
NOP
DEC AX
CMP AX,0
JNZ DELAY1
	RET
RANDOM:
pop cx
Push ds
XOR AX,AX
MOV DS,AX
XOR EAX,EAX
MOV AL,[DS:046Ch]
;PUSH EAX
mov dx,SEED
add dl,al
mov si,dx
mov al,[si]
pop ds
PUSH EAX
push cx
RET
