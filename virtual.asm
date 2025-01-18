;ATTENZIONE RICHIEDE "c:\krakow\graph.asm"
SEGMENT .SCREEN align=1
VSCREEN times 64000 db 0
SEGMENT .text public class=code align=1
FLIP:
PUSH DS
MOV AX, 0XA000
MOV ES, AX; { ES = SEGMENT OF SOURCE }
MOV AX, seg VSCREEN
MOV DS, AX ; { DS = SEGMENT OF SOURCE }
xor SI, si ; { SI = 0 FASTER THEN MOV SI,0 }
XOR DI, DI ; { DI = 0 }
MOV CX, 32000
REP MOVSW ; { REPEAT MOVSW 32000 TIMES }
POP DS
RET
VFLIP:
PUSH DS
MOV AX, seg VSCREEN
MOV ES, AX; { ES = SEGMENT OF SOURCE }
MOV AX, 0xa000
MOV DS, AX ; { DS = SEGMENT OF SOURCE }
xor SI, si ; { SI = 0 FASTER THEN MOV SI,0 }
XOR DI, DI ; { DI = 0 }
MOV CX, 32000
REP MOVSW ; { REPEAT MOVSW 32000 TIMES }
POP DS
RET

VGETPIXEL:
MOV BX,[X]
MOV CX,[Y]
ADD BH,CL
SHL CX,6
ADD BX,CX
MOV AX,seg VSCREEN
MOV ES,AX
MOV AL,[ES:BX]
MOV [C],AL
RET
VPUTPIXEL:
MOV AX, seg VSCREEN
MOV ES,AX
MOV BX,[X]
MOV DX,[Y]
MOV DI,BX
MOV BX, DX
SHL DX, 8
SHL BX, 6
ADD DX, BX
ADD DI, DX
MOV AL, [C]
STOSB
RET
VCLS:
MOV AX, seg VSCREEN
MOV ES,AX
XOR DI,DI
MOV AL, [C]
MOV AH,AL
MOV CX,32000
REP STOSW
RET
VCERCHIO:
xor eax,eax
mov ax,[R]
PUSH EAX
MOV EAX,1
POP EDX
CMP EAX,EDX
jg fgL11
jmp flL11
fgL11:
mov eax,0xffffffff
flL11:
CMP EAX,0FFFFFFFFh
je n11
Jmp L12
n11:
MOV EAX,1
mov [R],ax
L12:
MOV EAX,0
mov [IX],ax
xor eax,eax
mov ax,[R]
PUSH EAX
MOV EAX,64
POP ECX
MUL ECX
mov [IY],ax
xor eax,eax
mov ax,[X]
mov [X1],ax
xor eax,eax
mov ax,[Y]
mov [Y1],ax
L13:
xor eax,eax
mov ax,[IX]
PUSH EAX
MOV EAX,32
POP EDX
ADD EAX,EDX
PUSH EAX
MOV EAX,64
POP ECX
XOR EDX,EDX
XCHG ECX,EAX
DIV ECX
mov [A],ax
xor eax,eax
mov ax,[IY]
PUSH EAX
MOV EAX,32
POP EDX
ADD EAX,EDX
PUSH EAX
MOV EAX,64
POP ECX
XOR EDX,EDX
XCHG ECX,EAX
DIV ECX
mov [B],ax
xor eax,eax
mov ax,[X1]
PUSH EAX
xor eax,eax
mov ax,[A]
POP EDX
ADD EAX,EDX
mov [X],ax
xor eax,eax
mov ax,[Y1]
PUSH EAX
xor eax,eax
mov ax,[B]
POP EDX
ADD EAX,EDX
mov [Y],ax
CALL VPUTPIXEL
xor eax,eax
mov ax,[Y1]
PUSH EAX
xor eax,eax
mov ax,[B]
POP EDX
SUB EAX,EDX
NEG EAX
mov [Y],ax
CALL PUTPIXEL
xor eax,eax
mov ax,[X1]
PUSH EAX
xor eax,eax
mov ax,[A]
POP EDX
SUB EAX,EDX
NEG EAX
mov [X],ax
CALL VPUTPIXEL
xor eax,eax
mov ax,[Y1]
PUSH EAX
xor eax,eax
mov ax,[B]
POP EDX
ADD EAX,EDX
mov [Y],ax
CALL VPUTPIXEL
xor eax,eax
mov ax,[X1]
PUSH EAX
xor eax,eax
mov ax,[B]
POP EDX
ADD EAX,EDX
mov [X],ax
xor eax,eax
mov ax,[Y1]
PUSH EAX
xor eax,eax
mov ax,[A]
POP EDX
ADD EAX,EDX
mov [Y],ax
CALL VPUTPIXEL
xor eax,eax
mov ax,[Y1]
PUSH EAX
xor eax,eax
mov ax,[A]
POP EDX
SUB EAX,EDX
NEG EAX
mov [Y],ax
CALL VPUTPIXEL
xor eax,eax
mov ax,[X1]
PUSH EAX
xor eax,eax
mov ax,[B]
POP EDX
SUB EAX,EDX
NEG EAX
mov [X],ax
CALL VPUTPIXEL
xor eax,eax
mov ax,[Y1]
PUSH EAX
xor eax,eax
mov ax,[A]
POP EDX
ADD EAX,EDX
mov [Y],ax
CALL VPUTPIXEL
xor eax,eax
mov ax,[IX]
PUSH EAX
xor eax,eax
mov ax,[IY]
PUSH EAX
xor eax,eax
mov ax,[R]
POP ECX
XOR EDX,EDX
XCHG ECX,EAX
DIV ECX
POP EDX
ADD EAX,EDX
mov [IX],ax
xor eax,eax
mov ax,[IY]
PUSH EAX
xor eax,eax
mov ax,[IX]
PUSH EAX
xor eax,eax
mov ax,[R]
POP ECX
XOR EDX,EDX
XCHG ECX,EAX
DIV ECX
POP EDX
SUB EAX,EDX
NEG EAX
mov [IY],ax
xor eax,eax
mov ax,[B]
PUSH EAX
xor eax,eax
mov ax,[A]
POP EDX
CMP EAX,EDX
jge gL15
jmp lL15
gL15:
mov eax,0xffffffff
lL15:
CMP EAX,0FFFFFFFFh
jne n12
Jmp L14
n12:
JMP L13
L14:
RET
VOLINE:
MOV AX, seg VSCREEN
MOV ES,AX
MOV AX,[Y]
MOV DI,AX
SHL AX,8
SHL DI,6
ADD DI,AX
ADD DI,[X]
MOV AL,[C]
MOV AH,AL
MOV CX,[X1]
SUB CX,[X]
SHR CX,1
JNC VVLINEX
STOSB
VVLINEX:
REP STOSW
RET
VVLINE:
xor eax,eax
mov ax,[Y]
PUSH EAX
xor eax,eax
mov ax,[Y1]
POP EDX
CMP EAX,EDX
jg gL16
jmp lL16
gL16:
mov eax,0xffffffff
lL16:
CMP EAX,0FFFFFFFFh
je n13
Jmp L17
n13:
xor eax,eax
mov ax,[Y]
mov [TMP],ax
L18t:
xor eax,eax
mov ax,[Y]
PUSH EAX
xor eax,eax
mov ax,[Y1]
POP EDX
CMP EAX,EDX
jge gL19
jmp lL19
gL19:
mov eax,0xffffffff
lL19:
CMP EAX,0FFFFFFFFh
je n14
Jmp L18f
n14:
JMP L18b
L18a:
xor eax,eax
mov ax,[Y]
PUSH EAX
MOV EAX,1
POP EDX
ADD EAX,EDX
mov [Y],ax
JMP L18t
L18b:
CALL VPUTPIXEL
JMP L18a
L18f:
JMP L20
L17:
xor eax,eax
mov ax,[Y]
mov [TMP],ax
L21t:
xor eax,eax
mov ax,[Y]
PUSH EAX
xor eax,eax
mov ax,[Y1]
POP EDX
CMP EAX,EDX
jle gL22
jmp lL22
gL22:
mov eax,0xffffffff
lL22:
CMP EAX,0FFFFFFFFh
je n15
Jmp L21f
n15:
JMP L21b
L21a:
xor eax,eax
mov ax,[Y]
PUSH EAX
MOV EAX,1
POP EDX
SUB EAX,EDX
NEG EAX
mov [Y],ax
JMP L21t
L21b:
CALL VPUTPIXEL
JMP L21a
L21f:
L20:
RET