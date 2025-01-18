; originally posted 13/04/2004 https://www.freeforumzone.com/mobile/d/2925993/LaPasseraSoftware-Presenta-KraKow/discussione.aspx?p=1&pl=4&idm1=37268082
SEGMENT .data
TMP	Dw 0
R	Dw 0
A	Dw 0
B	Dw 0
IX	Dw 0
IY	Dw 0
X	Dw 0
X1	Dw 0
Y	Dw 0
Y1	Dw 0
C	Db 0
VERDE	Db 0
ROSSO	Db 0
BLU	Db 0
SEGMENT .text
jmp Start
GETPAL:
MOV DX,0X3C7
MOV AL,[C]                                                                      
OUT DX,AL                                                                       
MOV DX,0X3C9                                                                    
IN AL,DX                                                                        
MOV [ROSSO],AL                                                                  
IN AL,DX                                                                        
MOV [VERDE],AL                                                                  
IN AL,DX                                                                        
MOV [BLU],AL                                                                    
	RET
PAL: 
MOV DX,0X3C8                                                                    
MOV AL,[C]                                                                      
OUT DX,AL                                                                       
MOV DX,0X3C9                                                                    
MOV AL,[ROSSO]                                                                  
OUT DX,AL                                                                       
MOV AL,[VERDE]
OUT DX,AL                                                                       
MOV AL,[BLU]                                                                    
OUT DX,AL                                                                       
	RET
SETMODEX: 
MOV AX,0013H                                                                    
INT 10H                                                                         
	RET
RESTORE: 
MOV AX,0003H                                                                    
INT 10H                                                                         
	RET
GETPIXEL: 
MOV BX,[X]                                                                      
MOV CX,[Y]                                                                      
ADD BH,CL                                                                       
SHL CX,6                                                                        
ADD BX,CX
MOV AX,0XA000                                                                   
MOV ES,AX                                                                       
MOV AL,[ES:BX]                                                                  
MOV [C],AL                                                                      
	RET
PUTPIXEL: 
MOV     AX,0XA000                                                               
MOV     ES,AX                                                                   
MOV     BX,[X]                                                                  
MOV     DX,[Y]                                                                  
MOV     DI,BX                                                                   
MOV     BX, DX                                                                  
SHL     DX, 8                                                                   
SHL     BX, 6                                                                   
ADD     DX, BX                                                                  
ADD     DI, DX                                                                  
MOV     AL, [C]                                                                 
STOSB
	RET
CLS: 
MOV     AX,0XA000                                                               
MOV     ES,AX                                                                   
XOR     DI,DI                                                                   
MOV     AL, [C]                                                                 
MOV AH,AL                                                                       
MOV CX,32000                                                                    
REP STOSW                                                                       
	RET
WAITRETRACE: 
MOV DX,3DAH                                                                     
WAITRETER1:                                                                     
IN AL,DX                                                                        
AND AL,08H                                                                      
JNZ WAITRETER1                                                                  
WAITRETER2:                                                                     
IN AL,DX
AND AL,08H                                                                      
JZ  WAITRETER2                                                                  
	RET
CERCHIO: 
	xor eax,eax
	mov ax,[R]
	PUSH EAX
	MOV EAX,1
	POP EDX
	CMP EAX,EDX
	jg gL0
	jmp lL0
	gL0:
	mov eax,0xffffffff
lL0:
	CMP EAX,0FFFFFFFFh
	je n0
	Jmp L1
	n0:
	MOV EAX,1
	mov [R],ax
L1:
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
L2:
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
	CALL PUTPIXEL
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
	CALL PUTPIXEL
	xor eax,eax
	mov ax,[Y1]
	PUSH EAX
	xor eax,eax
	mov ax,[B]
	POP EDX
	ADD EAX,EDX
	mov [Y],ax
	CALL PUTPIXEL
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
	CALL PUTPIXEL
	xor eax,eax
	mov ax,[Y1]
	PUSH EAX
	xor eax,eax
	mov ax,[A]
	POP EDX
	SUB EAX,EDX
	NEG EAX
	mov [Y],ax
	CALL PUTPIXEL
	xor eax,eax
	mov ax,[X1]
	PUSH EAX
	xor eax,eax
	mov ax,[B]
	POP EDX
	SUB EAX,EDX
	NEG EAX
	mov [X],ax
	CALL PUTPIXEL
	xor eax,eax
	mov ax,[Y1]
	PUSH EAX
	xor eax,eax
	mov ax,[A]
	POP EDX
	ADD EAX,EDX
	mov [Y],ax
	CALL PUTPIXEL
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
	jge gL4
	jmp lL4
	gL4:
	mov eax,0xffffffff
lL4:
	CMP EAX,0FFFFFFFFh
	jne n1
	Jmp L3
	n1:
	JMP L2
L3:
	RET
OLINE: 
MOV AX,0XA000                                                                   
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
JNC VLINEX                                                                      
STOSB                                                                           
VLINEX:                                                                         
REP STOSW                                                                       
	RET
VLINE: 
	xor eax,eax
	mov ax,[Y]
	PUSH EAX
	xor eax,eax
	mov ax,[Y1]
	POP EDX
	CMP EAX,EDX
	jg gL5
	jmp lL5
	gL5:
	mov eax,0xffffffff
lL5:
	CMP EAX,0FFFFFFFFh
	je n2
	Jmp L6
	n2:
	xor eax,eax
	mov ax,[Y]
	mov [TMP],ax
L7t:
	xor eax,eax
	mov ax,[Y]
	PUSH EAX
	xor eax,eax
	mov ax,[Y1]
	POP EDX
	CMP EAX,EDX
	jge gL8
	jmp lL8
	gL8:
	mov eax,0xffffffff
lL8:
	CMP EAX,0FFFFFFFFh
	je n3
	Jmp L7f
	n3:
	JMP L7b
L7a:
	xor eax,eax
	mov ax,[Y]
	PUSH EAX
	MOV EAX,1
	POP EDX
	ADD EAX,EDX
	mov [Y],ax
	JMP L7t
L7b:
	CALL PUTPIXEL
	JMP L7a
L7f:
	JMP L9
L6:
	xor eax,eax
	mov ax,[Y]
	mov [TMP],ax
L10t:
	xor eax,eax
	mov ax,[Y]
	PUSH EAX
	xor eax,eax
	mov ax,[Y1]
	POP EDX
	CMP EAX,EDX
	jle gL11
	jmp lL11
	gL11:
	mov eax,0xffffffff
lL11:
	CMP EAX,0FFFFFFFFh
	je n4
	Jmp L10f
	n4:
	JMP L10b
L10a:
	xor eax,eax
	mov ax,[Y]
	PUSH EAX
	MOV EAX,1
	POP EDX
	SUB EAX,EDX
	NEG EAX
	mov [Y],ax
	JMP L10t
L10b:
	CALL PUTPIXEL
	JMP L10a
L10f:
L9:
	RET
;licenza GPL proprieta intellettuale di kentaromiura alias 
;carlesso cristian