{originally posted in 14/04/2004 https://www.freeforumzone.com/mobile/d/2925995/krakow-che-genera-exe/discussione.aspx}
{--------------------------------------------------------------}
program Krakow;

{--------------------------------------------------------------}
{ Constant Declarations }

const TAB = ^I;
      CR  = ^M;
      LF  = ^J;

      Farlab: integer = 0;(*Aggiunto x gestire salti di tipo FAR*)
      LCount: integer = 0;
      NEntry: integer = 0;


{--------------------------------------------------------------}
{ Type Declarations }

type Symbol = string[16];

     SymTab = array[1..1000] of Symbol;

     TabPtr = ^SymTab;


{--------------------------------------------------------------}
{ Variable Declarations }

var iniz : boolean;
    ciclo: integer;
    proc:string;{proc attuale}
    Look : char;             { Lookahead Character }
    Token: char;             { Encoded Token       }
    Value: string[16];       { Unencoded Token     }
{    sasm: string;}
    sasm:string[80];
const MaxEntry = 100;

var ST   : array[1..MaxEntry] of Symbol;
    SType: array[1..MaxEntry] of char;


{--------------------------------------------------------------}
{ Definition of Keywords and Token Types }

const NKW =  32;
      NKW1 = 33;

const KWlist: array[1..NKW] of Symbol =
('SE', 'INVECE', 'FSE', 'MENTRE', 'FMENTRE','RIPETI','FINCHE','LEGGI',
 'SCRIVI', 'VAR', 'FINE','LOOP','FLOOP','WORD','BYTE','LONG','LAST','PER','FPER','REP',
 'FREP','AST','FAST','STAMPAB','ASM','FASM','SUB','FSUB','SUBR','FSUBR','RETURN','IMPORTA');

const KWcode: string[NKW1] = 'xileweqeRWvezehjkfPeOeEeBaeSereXI';

function Typeof(N:integer):symbol;
begin
Typeof:=ST[N];
end;

{--------------------------------------------------------------}
{ Read New Character From Input Stream }
procedure GetChar; forward;
procedure Store(Name: Symbol);forward;
procedure GetCharX;
begin
   Read(Look);
end;

{--------------------------------------------------------------}
{ Report an Error }

procedure Error(s: string);
begin
   WriteLn;
   WriteLn(^G, 'Error: ', s, '.');
end;


{--------------------------------------------------------------}
{ Report Error and Halt }

procedure Abort(s: string);
begin
   Error(s);
   Halt;
end;


{--------------------------------------------------------------}
{ Report What Was Expected }

procedure Expected(s: string);
begin
   Abort(s + ' Expected');
end;

{--------------------------------------------------------------}
{ Report an Undefined Identifier }

procedure Undefined(n: string);
begin
   Abort('Undefined Identifier ' + n);
end;


{--------------------------------------------------------------}
{ Report a Duplicate Identifier }

procedure Duplicate(n: string);
begin
   Abort('Duplicate Identifier ' + n);
end;


{--------------------------------------------------------------}
{ Check to Make Sure the Current Token is an Identifier }

procedure CheckIdent;
begin
   if Token <> 'x' then Expected('Identifier');
end;


{--------------------------------------------------------------}
{ Recognize an Alpha Character }

function IsAlpha(c: char): boolean;
begin
   IsAlpha := UpCase(c) in ['A'..'Z'];
end;


{--------------------------------------------------------------}
{ Recognize a Decimal Digit }

function IsDigit(c: char): boolean;
begin
   IsDigit := c in ['0'..'9'];
end;


{--------------------------------------------------------------}
{ Recognize an AlphaNumeric Character }

function IsAlNum(c: char): boolean;
begin
   IsAlNum := IsAlpha(c) or IsDigit(c);
end;


{--------------------------------------------------------------}
{ Recognize an Addop }

function IsAddop(c: char): boolean;
begin
   IsAddop := c in ['+', '-'];
end;


{--------------------------------------------------------------}
{ Recognize a Mulop }

function IsMulop(c: char): boolean;
begin
   IsMulop := c in ['*', '/'];
end;


{--------------------------------------------------------------}
{ Recognize a Boolean Orop }

function IsOrop(c: char): boolean;
begin
   IsOrop := c in ['|', '~'];
end;

function Isstring(c: char): boolean;
begin
   Isstring := not (c in ['"']);
end;


{--------------------------------------------------------------}
{ Recognize a Relop }

function IsRelop(c: char): boolean;
begin
   IsRelop := c in ['=', '#', '<', '>'];
end;


{--------------------------------------------------------------}
{ Recognize White Space }

function IsWhite(c: char): boolean;
begin
   IsWhite := c in [' ', TAB, CR, LF];
end;


{--------------------------------------------------------------}
{ Skip Over Leading White Space }
procedure SkipWhite;
begin
   while IsWhite(Look) do
      GetChar;
end;


{--------------------------------------------------------------}
{ Table Lookup }

function Lookup(T: TabPtr; s: string; n: integer): integer;
var i: integer;
    found: Boolean;
begin
   found := false;
   i := n;
   while (i > 0) and not found do
      if s = T^[i] then
         found := true
      else
         dec(i);
   Lookup := i;
end;


{--------------------------------------------------------------}
{ Locate a Symbol in Table }
{ Returns the index of the entry.  Zero if not present. }

function Locate(N: Symbol): integer;
begin
   Locate := Lookup(@ST, n, NEntry);
end;


{--------------------------------------------------------------}
{ Look for Symbol in Table }

function InTable(n: Symbol): Boolean;
begin
   InTable := Lookup(@ST, n, NEntry) <> 0;
end;


{--------------------------------------------------------------}
{ Check to See if an Identifier is in the Symbol Table         }
{ Report an error if it's not. }


procedure CheckTable(N: Symbol);
begin
   if not InTable(N) then Undefined(N);
end;


{--------------------------------------------------------------}
{ Check the Symbol Table for a Duplicate Identifier }
{ Report an error if identifier is already in table. }


procedure CheckDup(N: Symbol);
begin
   if InTable(N) then Duplicate(N);
end;


{--------------------------------------------------------------}
{ Add a New Entry to Symbol Table }

procedure AddEntry(N: Symbol; T: char);
begin
   CheckDup(N);
   if NEntry = MaxEntry then Abort('Symbol Table Full');
   Inc(NEntry);
   ST[NEntry] := N;
   SType[NEntry] := T;
end;


{--------------------------------------------------------------}
{ Get an Identifier }

procedure GetName;
begin
   SkipWhite;
   if Not IsAlpha(Look) then Expected('Identifier');
   Token := 'x';
   Value := '';
   repeat
      Value := Value + UpCase(Look);
      GetChar;
   until not IsAlNum(Look);
end;

procedure GetsubName;
begin
   SkipWhite;
   if Not IsAlpha(Look) then Expected('sub name');
   Token := 'S';
   Value := '';
   repeat
      Value := Value + UpCase(Look);
      GetChar;
   until not IsAlNum(Look);
end;


procedure stringa;
begin
   SkipWhite;
   Token := 'x';
   Value := '';
   repeat
      Value := Value + Look;
      GetChar;
   until not Isstring(Look);
end;


{--------------------------------------------------------------}
{ Get a Number }
procedure GetNum;
begin
   SkipWhite;
   if not IsDigit(Look) then Expected('Number');
   Token := '#';
   Value := '';
   repeat
      Value := Value + Look;
      GetChar;
   until not IsDigit(Look);
end;


{--------------------------------------------------------------}
{ Get an Operator }

procedure GetOp;
begin
   SkipWhite;
   Token := Look;
   Value := Look;
   GetChar;
end;


{--------------------------------------------------------------}
{ Get the Next Input Token }

procedure Next;
begin
   SkipWhite;
   if IsAlpha(Look) then GetName
   else if IsDigit(Look) then GetNum
   else GetOp;
end;


{--------------------------------------------------------------}
{ Scan the Current Identifier for Keywords }

procedure Scan;
begin
   if Token = 'x' then
      Token := KWcode[Lookup(Addr(KWlist), Value, NKW) + 1];
end;


{--------------------------------------------------------------}
{ Match a Specific Input String }

procedure MatchString(x: string);
begin
   if Value <> x then Expected('''' + x + '''');
   Next;
end;


{--------------------------------------------------------------}
{ Output a String with Tab }

procedure Emit(s: string);
begin
   Write(TAB, s);
end;


{--------------------------------------------------------------}
{ Output a String with Tab and CRLF }

procedure EmitLn(s: string);
begin
   Emit(s);
   WriteLn;
end;


{--------------------------------------------------------------}
{ Generate a Unique Label }

function NewLabel: string;
var S: string;
begin
   Str(LCount, S);
   NewLabel := 'L' + S;
   Inc(LCount);
end;
function Newfar: string;
var S: string;
begin
   Str(farlab, S);
   Newfar := 'n' + S;
   Inc(farlab);
end;


{--------------------------------------------------------------}
{ Post a Label To Output }

procedure PostLabel(L: string);
begin
   WriteLn(L, ':');
end;


{---------------------------------------------------------------}
{ Clear the Primary Register }
procedure Clear;
begin
   (*EmitLn('CLR D0');*)
   Emitln('XOR EAX,EAX');
end;

{
procedure Clear;
begin
   (*EmitLn('CLR D0');*)
   Emitln('XOR AX,AX');
end;
}

{---------------------------------------------------------------}
{ Negate the Primary Register }
procedure Negate;
begin
(*   EmitLn('NEG D0');*)
Emitln('NEG EAX');
end;

{
procedure Negate;
begin
(*   EmitLn('NEG D0');*)
Emitln('NEG AX');
end;
}

{---------------------------------------------------------------}
{ Complement the Primary Register }


procedure NotIt;
begin
(*   EmitLn('NOT D0');*)
Emitln('NOT EAX');
end;
{
procedure NotIt;
begin
(*   EmitLn('NOT D0');*)
Emitln('NOT AX');
end;
}

{---------------------------------------------------------------}
{ Load a Constant Value to Primary Register }

procedure LoadConst(n: string);
begin
(*   Emit('MOVE #');
   WriteLn(n, ',D0');*)
   Emit('MOV EAX,');
   WriteLn(n);

end;


{---------------------------------------------------------------}
{ Load a Variable to Primary Register }
procedure LoadVar(Name: symbol);forward;
{
procedure LoadVar(Name: string);
begin
   if not InTable(Name) then Undefined(Name);
 (*  EmitLn('MOVE ' + Name + '(PC),D0');*)(*da fare .*)
    EmitLn('MOV AX,[' + Name + ']');
end;
 }

{---------------------------------------------------------------}
{ Push Primary onto Stack }
procedure Push;
begin
Emitln('PUSH EAX');
end;
{
procedure Push;
begin
(*   EmitLn('MOVE D0,-(SP)');*)
     Emitln('PUSH AX');
end;
}

{---------------------------------------------------------------}
{ Add Top of Stack to Primary }

procedure PopAdd;
begin
Emitln('POP EDX');
Emitln('ADD EAX,EDX');
(*   EmitLn('ADD (SP)+,D0');*)
end;

{
procedure PopAdd;
begin
Emitln('POP DX');
Emitln('ADD AX,DX');
(*   EmitLn('ADD (SP)+,D0');*)
end;
}

{---------------------------------------------------------------}
{ Subtract Primary from Top of Stack }
procedure PopSub;
begin
(*   EmitLn('SUB (SP)+,D0');
   EmitLn('NEG D0');*)
   Emitln('POP EDX');
   Emitln('SUB EAX,EDX');
   EMitln('NEG EAX');
end;

{
procedure PopSub;
begin
(*   EmitLn('SUB (SP)+,D0');
   EmitLn('NEG D0');*)
   Emitln('POP DX');
   Emitln('SUB AX,DX');
   EMitln('NEG AX');
end;
}

{---------------------------------------------------------------}
{ Multiply Top of Stack by Primary }

procedure PopMul;
begin
(* EmitLn('MULS (SP)+,D0');*)
Emitln('POP ECX');
Emitln('MUL ECX');
end;

{
procedure PopMul;
begin
(* EmitLn('MULS (SP)+,D0');*)
Emitln('POP CX');
Emitln('MUL CX');
end;
}

{---------------------------------------------------------------}
{ Divide Top of Stack by Primary }
procedure PopDiv;
begin
(*
   EmitLn('MOVE (SP)+,D7');
   EmitLn('EXT.L D7');
   EmitLn('DIVS D0,D7');
   EmitLn('MOVE D7,D0');
   *)
EmitLn('POP ECX');
EmitLn('XOR EDX,EDX');
Emitln('XCHG ECX,EAX');
EmitLn('DIV ECX');
end;

{
procedure PopDiv;
begin
(*
   EmitLn('MOVE (SP)+,D7');
   EmitLn('EXT.L D7');
   EmitLn('DIVS D0,D7');
   EmitLn('MOVE D7,D0');
   *)
EmitLn('POP CX');
EmitLn('XOR DX,DX');
Emitln('XCHG CX,AX');
EmitLn('DIV CX');

end;}
procedure expression; forward;

procedure writeitb;
var a:string;
begin
a:=newlabel;
writeln('SEGMENT .data public class=data align=1');
emitln(a+' db "   $"');
writeln('SEGMENT .text public class=code align=1');
emitln('mov cl,100');
emitln('div cl');
emitln('add al,48');
emitln('mov ['+a+'],al');
emitln('cmp ah,0');
emitln('jne g'+a);    (*                         *)
emitln('jmp '+a+'e'); (*  Questo sostituisce je  *)
emitln('g'+a+':');    (*                         *)
emitln('mov al,ah');
emitln('xor ah,ah');
emitln('mov cl,10');
emitln('div cl');
emitln('add al,48');
emitln('add ah,48');
emitln('mov ['+a+'+1],al');
emitln('mov ['+a+'+2],ah');
emitln(a+'e:');
emitln('mov cx,"$"');
emitln('mov al,['+a+']');
emitln('cmp al,48');
emitln('je w'+a);
EMITLN('Jmp '+A+'g');{**}
emitln('w'+a+':');
EMITLN('MOV AL,['+A+'+1]');
emitln('mov ['+a+'],al');
emitln('mov al,['+a+'+2]');
emitln('mov ['+a+'+1],al');
emitln('mov ['+a+'+2],cl');
writeln(a+'g:');
emitln('mov al,['+a+']');
emitln('cmp al,48');
emitln('je d'+a);
emitln('jmp '+a+'g1');{**}
emitln('d'+a+':');
emitln('mov al,['+a+'+1]');
emitln('mov ['+a+'],al');
emitln('mov al,['+a+'+2]');
emitln('mov ['+a+'+1],al');
emitln('mov ['+a+'+2],cl');
writeln(a+'g1:');
emitln('mov ['+a+'+3],cl');
emitln('mov dx,'+a);
emitln('mov ax,'+a);
emitln('mov ah,9h');
emitln('int 21h');
end;
procedure stampab;
begin
   Next;
   MatchString('(');
   Expression;
   WriteItb;
   while Token = ',' do begin
      Next;
      Expression;
      WriteItb;
   end;
   MatchString(')');
end;
{---------------------------------------------------------------}
{ AND Top of Stack with Primary }
procedure PopAnd;
begin
(*
   EmitLn('AND (SP)+,D0');*)
   EmitLn('POP EDX');
   EmitLn('AND EAX,EDX');
end;

{
procedure PopAnd;
begin
(*
   EmitLn('AND (SP)+,D0');*)
   EmitLn('POP DX');
   EmitLn('AND AX,DX');
end;
}

{---------------------------------------------------------------}
{ OR Top of Stack with Primary }

procedure PopOr;
begin
(*
   EmitLn('OR (SP)+,D0');*)
   EmitLn('POP EDX');
   EmitLn('OR EAX,EDX');
end;
{
procedure PopOr;
begin
(*
   EmitLn('OR (SP)+,D0');*)
   EmitLn('POP DX');
   EmitLn('OR AX,DX');
end;
}

{---------------------------------------------------------------}
{ XOR Top of Stack with Primary }
procedure PopXor;
begin
(*
   EmitLn('EOR (SP)+,D0');*)
   Emitln('POP EDX');
   EmitLn('XOR EAX,EDX');
end;

{
procedure PopXor;
begin
(*
   EmitLn('EOR (SP)+,D0');*)
   Emitln('POP DX');
   EmitLn('XOR AX,DX');
end;
}

{---------------------------------------------------------------}
{ Compare Top of Stack with Primary }
procedure PopCompare;
begin
(*
   EmitLn('CMP (SP)+,D0');*)
   EmitLn('POP EDX');
   EmitLN('CMP EAX,EDX');
end;

{
procedure PopCompare;
begin
(*
   EmitLn('CMP (SP)+,D0');*)
   EmitLn('POP DX');
   EmitLN('CMP AX,DX');
end;
}

{---------------------------------------------------------------}
{ Set D0 If Compare was = }

procedure SetEqual;
var d:string;
begin
d:=newlabel;
(*   EmitLn('SEQ D0');
   EmitLn('EXT D0');*)
   emitln('je g'+d);  (*jne*)
   emitln('jmp l'+d);
   emitln('g'+d+':');
   EmitLn('mov EAx,0xffffffff');
   postlabel('l'+d);
end;


{---------------------------------------------------------------}
{ Set D0 If Compare was != }

procedure SetnEqual;
var s:string;
begin
s:=newlabel;
   (*EmitLn('SNE D0');
   EmitLn('EXT D0');*)
   emitln('jne g'+s);
   EmitLn('jmp l'+s);  {je}
   emitln('g'+s+':');
   Emitln('mov eax,0xffffffff');
   postlabel('l'+s);
end;


{---------------------------------------------------------------}
{ Set D0 If Compare was > }

procedure SetlESS;
var
s:string;
begin
s:=newlabel;
(*   EmitLn('SLT D0');
   EmitLn('EXT D0');*)
   emitln('jg g'+s);
   Emitln('jmp l'+s); {jng}
   emitln('g'+s+':');
   Emitln('mov eax,0xffffffff');
   postlabel('l'+s);
end;



{---------------------------------------------------------------}
{ Set D0 If Compare was < }

procedure SetgREATER;
var s:string;
begin
s:=newlabel;
(*   EmitLn('SGT D0');
   EmitLn('EXT D0');*)
   emitln('jl g'+s);
   EmitLn('jmp l'+s);     (*jnl*)
   emitln('g'+s+':');
   Emitln('mov eax,0xffffffff');
   postlabel('l'+s);
end;


{---------------------------------------------------------------}
{ Set D0 If Compare was <= }

procedure SetGREATEROrEqual;
var s:string;
begin
s:=newlabel;
(*   EmitLn('SGE D0');
   EmitLn('EXT D0');*)
   emitln('jle g'+s);
   Emitln('jmp l'+s);(*jnle*)
   emitln('g'+s+':');
   Emitln('mov eax,0xffffffff');
   postlabel('l'+s);
end;


{---------------------------------------------------------------}
{ Set D0 If Compare was >= }

procedure SetLESSOrEqual;
var s:string;
begin
s:=newlabel;
(*   EmitLn('SLE D0');
   EmitLn('EXT D0');*)
   emitln('jge g'+s);
   Emitln('jmp l'+s);(*jnge*)
   emitln('g'+s+':');
   Emitln('mov eax,0xffffffff');
   postlabel('l'+s);
end;


{---------------------------------------------------------------}
{ Store Primary to Variable }
{*
procedure Store(Name: string);
begin
(*
   EmitLn('LEA ' + Name + '(PC),A0');
   EmitLn('MOVE D0,(A0)')*)(*Da Fare*)
   EmitLn('MOV ['+Name+'],AX');
end;
  }

{---------------------------------------------------------------}
{ Branch Unconditional  }

procedure Branch(L: string);
begin
(*
   EmitLn('BRA ' + L);*)
   EmitLn('JMP ' + L);
end;


{---------------------------------------------------------------}
{ Branch False }

procedure BranchFalse(L: string);
var s:string;
begin
(*
   EmitLn('TST D0');
   EmitLn('BEQ ' + L);*)
s:=newfar;
   EmitLn('CMP EAX,0FFFFFFFFh');
   emitln('je '+s);
   EmitLn('Jmp ' + L);
   emitln(s+':');
end;
procedure BranchTrue(L: string);
var s:string;
begin
s:=newfar;
   EmitLn('CMP EAX,0FFFFFFFFh');
   emitln('jne '+s);
   EmitLn('Jmp ' + L);
   emitln(s+':');
end;


{---------------------------------------------------------------}
{ Read Variable to Primary Register }

procedure ReadIt(Name: string);
begin
Emitln('XOR AH,AH');
Emitln('INT 16h');
Emitln('xor ah,ah');
Emitln('mov ['+Name+'],ax');
(*

   EmitLn('BSR READ');
   Store(Name);*)(*da fare*)
end;


{ Write from Primary Register }

procedure WriteIt;
begin
Emitln('mov dl,al');
Emitln('mov ah,2');
Emitln('INT 21h');

(*   EmitLn('BSR WRITE');*)(*da fare*)
end;


{--------------------------------------------------------------}
{ Write Header Info }

procedure Header;
begin

(*   WriteLn('WARMST', TAB, 'EQU $A01E');*)(*Da Fare*)
end;


{--------------------------------------------------------------}
{ Write the Prolog }

procedure Prolog;
begin
{Writeln('SEGMENT .text');}
(*   PostLabel('MAIN');*)(*da fare*)
postlabel('..start');
PostLabel('Start');
emitln('mov ax,data');
emitln('mov ds,ax');
end;


{--------------------------------------------------------------}
{ Write the Epilog }

procedure Epilog;
begin
(*
   EmitLn('DC WARMST');
   EmitLn('END MAIN'); *)(*da fare*)
   Emitln('mov ax,4c00h');
   Emitln('int 21h');
   Emitln('segment .stack stack class=stack align=1');
   emitln('times 64000 db 0');
end;


{---------------------------------------------------------------}
{ Allocate Storage for a Static Variable }

procedure Allocate(Name, Val: string;kind:char);
begin
(*   WriteLn(Name, ':', TAB, 'DC ', Val);*)
if(iniz=false)then begin
Writeln('SEGMENT .data public class=data align=1');
iniz:=true;
end;

WriteLn(Name,TAB,'D'+kind+' ',Val);
end;


{---------------------------------------------------------------}
{ Parse and Translate a Math Factor }

procedure BoolExpression; Forward;

procedure Factor;
begin
   if Token = '(' then begin
      Next;
      BoolExpression;
      MatchString(')');
      end
   else begin
      if Token = 'x' then
         LoadVar(Value)
      else if Token = '#' then
         LoadConst(Value)
      else Expected('Math Factor');
      Next;
   end;
end;


{--------------------------------------------------------------}
{ Recognize and Translate a Multiply }

procedure Multiply;
begin
   Next;
   Factor;
   PopMul;
end;


{-------------------------------------------------------------}
{ Recognize and Translate a Divide }

procedure Divide;
begin
   Next;
   Factor;
   PopDiv;
end;


{---------------------------------------------------------------}
{ Parse and Translate a Math Term }

procedure Term;
begin
   Factor;
   while IsMulop(Token) do begin
      Push;
      case Token of
       '*': Multiply;
       '/': Divide;
      end;
   end;
end;


{--------------------------------------------------------------}
{ Recognize and Translate an Add }

procedure Add;
begin
   Next;
   Term;
   PopAdd;
end;


{-------------------------------------------------------------}
{ Recognize and Translate a Subtract }

procedure Subtract;
begin
   Next;
   Term;
   PopSub;
end;


{---------------------------------------------------------------}
{ Parse and Translate an Expression }

procedure Expression;
begin
   if IsAddop(Token) then
      Clear
   else
      Term;
   while IsAddop(Token) do begin
      Push;
      case Token of
       '+': Add;
       '-': Subtract;
      end;
   end;
end;


{---------------------------------------------------------------}
{ Get Another Expression and Compare }

procedure CompareExpression;
begin
   Expression;
   PopCompare;
end;


{---------------------------------------------------------------}
{ Get The Next Expression and Compare }

procedure NextExpression;
begin
   Next;
   CompareExpression;
end;


{---------------------------------------------------------------}
{ Recognize and Translate a Relational "Equals" }

procedure Equal;
begin
   NextExpression;
   SetEqual;
end;


{---------------------------------------------------------------}
{ Recognize and Translate a Relational "Less Than or Equal" }

procedure LessOrEqual;
begin
   NextExpression;
   SetLessOrEqual;
end;


{---------------------------------------------------------------}
{ Recognize and Translate a Relational "Not Equals" }

procedure NotEqual;
begin
   NextExpression;
   SetNEqual;
end;


{---------------------------------------------------------------}
{ Recognize and Translate a Relational "Less Than" }

procedure Less;
begin
   Next;
   case Token of
     '=': LessOrEqual;
     '>': NotEqual;
   else begin
           CompareExpression;
           SetLess;
        end;
   end;
end;


{---------------------------------------------------------------}
{ Recognize and Translate a Relational "Greater Than" }

procedure Greater;
begin
   Next;
   if Token = '=' then begin
      NextExpression;
      SetGreaterOrEqual;
      end
   else begin
      CompareExpression;
      SetGreater;
   end;
end;


{---------------------------------------------------------------}
{ Parse and Translate a Relation }


procedure Relation;
begin
   Expression;
   if IsRelop(Token) then begin
      Push;
      case Token of
       '=': Equal;
       '<': Less;
       '>': Greater;
      end;
   end;
end;


{---------------------------------------------------------------}
{ Parse and Translate a Boolean Factor with Leading NOT }

procedure NotFactor;
begin
   if Token = '!' then begin
      Next;
      Relation;
      NotIt;
      end
   else
      Relation;
end;


{---------------------------------------------------------------}
{ Parse and Translate a Boolean Term }

procedure BoolTerm;
begin
   NotFactor;
   while Token = '&' do begin
      Push;
      Next;
      NotFactor;
      PopAnd;
   end;
end;


{--------------------------------------------------------------}
{ Recognize and Translate a Boolean OR }

procedure BoolOr;
begin
   Next;
   BoolTerm;
   PopOr;
end;


{--------------------------------------------------------------}
{ Recognize and Translate an Exclusive Or }

procedure BoolXor;
begin
   Next;
   BoolTerm;
   PopXor;
end;


{---------------------------------------------------------------}
{ Parse and Translate a Boolean Expression }

procedure BoolExpression;
begin
   BoolTerm;
   while IsOrOp(Token) do begin
      Push;
      case Token of
       '|': BoolOr;
       '~': BoolXor;
      end;
   end;
end;


{--------------------------------------------------------------}
{ Parse and Translate an Assignment Statement }

procedure cal(s:string);
begin
emitln('CALL '+s);
end;
procedure Assignment;
var Name: string;
begin
   CheckTable(Value);
   Name := Value;
   Next;
   if stype[locate(Name)]='r' then Abort('subr can''t be assigned');
   if stype[locate(Name)]='S' then cal(Name)
   else begin
   MatchString('=');
   if stype[locate(Name)]='r' then cal(Name)
   else BoolExpression;
   Store(Name);
   end;
end;

{PROCEDURE assignstring;
var Name: string;
begin
   CheckTable(Value);
   Name := Value;
   Next;
   MatchString('=');
   MatchString('"');
   Stringa;
   MatchString('"');
   Stores(Name);
end;
}
{---------------------------------------------------------------}
{ Recognize and Translate an IF Construct }

procedure Block; Forward;

procedure DoIf;
var L1, L2: string;
begin
   Next;
   BoolExpression;
   L1 := NewLabel;
   L2 := L1;
   BranchFalse(L1);
   Block;
   if Token = 'l' then begin
      Next;
      L2 := NewLabel;
      Branch(L2);
      PostLabel(L1);
      Block;
   end;
   PostLabel(L2);
   MatchString('FSE');
end;


{--------------------------------------------------------------}
{ Parse and Translate a WHILE Statement }
procedure loop;
var l1,l2:string;
begin
next;
l1:=Newlabel;
ciclo:=lcount;
l2:=Newlabel;
postlabel(l1);
block;
matchstring('FLOOP');
postlabel(l2);
end;

procedure brek;
var l:string;
begin
next;
str(ciclo,l);
if ciclo<lcount then
branch('L'+l);
end;

procedure DoWhile;
var L1, L2: string;
begin
   Next;
   L1 := NewLabel;
   ciclo:=lcount;
   L2 := NewLabel;
   PostLabel(L1);
   BoolExpression;
   BranchFalse(L2);
   Block;
   MatchString('FMENTRE');
   Branch(L1);
   PostLabel(L2);
end;

procedure repeatun;
var L1, L2: string;
begin
   Next;
   L1 := NewLabel;
   ciclo:=lcount;
   L2 := NewLabel;
   PostLabel(L1);
   block;
   MatchString('FINCHE');
   BoolExpression;
   Branchtrue(L2);
   Branch(L1);
   PostLabel(L2);

end;


{--------------------------------------------------------------}
{ Read a Single Variable }

procedure ReadVar;
begin
   CheckIdent;
   CheckTable(Value);
   ReadIt(Value);
   Next;
end;


{--------------------------------------------------------------}
{ Process a Read Statement }

procedure DoRead;
begin
   Next;
   MatchString('(');
   ReadVar;
   while Token = ',' do begin
      Next;
      ReadVar;
   end;
   MatchString(')');
end;
{--------------------------------------------------------------}
{ Process a Write Statement }
procedure per;
var lab:string;
begin
lab:=newlabel;
   Next;
   MatchString('(');
   assignment;
postlabel(lab+'t') ;(*'L'+lab+'t'*)
   MatchString(':');
   BoolExpression;
   Branchfalse(lab+'f');
   branch(lab+'b');
   MatchString(':');
postlabel(lab+'a');
   assignment;
   branch(lab+'t');
postlabel(lab+'b');
   MatchString(')');
   block;
   branch(lab+'a');
postlabel(lab+'f');
   matchString('FPER');
end;
procedure ter;
var lab:string;
begin
lab:=newlabel;
   Next;
   boolexpression;
   branchfalse(lab);
   MatchString(':');
   assignment;
branch(lab+'e');
postlabel(lab);
   MatchString(':');
   assignment;
   MatchString('FAST');
postlabel(lab+'e');
end;

procedure rep;
var lab:string;
begin
lab:=newlabel;
   Next;
   MatchString('(');
   assignment;
postlabel(lab+'t') ;(*'L'+lab+'t'*)
   MatchString(':');
   BoolExpression;
   Branchtrue(lab+'f');
   branch(lab+'b');
   MatchString(':');
postlabel(lab+'a');
   assignment;
   branch(lab+'t');
postlabel(lab+'b');
   MatchString(')');
   block;
   branch(lab+'a');
postlabel(lab+'f');
   matchString('FREP');
end;
{
procedure doasm;
begin
getasm;
while(sasm<>'FASM') do begin
writeln( sasm );
getasm;
end;
end;
}
procedure DoWrite;
begin
   Next;
   MatchString('(');
   Expression;
   WriteIt;
   while Token = ',' do begin
      Next;
      Expression;
      WriteIt;
   end;
   MatchString(')');
end;



{ Skip A Comment Field }

procedure SkipComment;
begin
   while Look <> '}' do
      GetCharX;
   GetCharX;
end;
procedure lineSkipComment;
begin
   while Look <> CR do
      GetCharX;
   GetCharX;
end;

{--------------------------------------------------------------}
{ Get Character from Input Stream }
{ Skip Any Comments }

procedure GetChar;
begin
   GetCharX;
   case Look of
    '{' : SkipComment;
    ';' : lineSkipComment(*se non funziona e colpa mia*)
    end;
end;
{--------------------------------------------------------------}
{procedure SkipWhitez;
begin
   while IsWhite(Look) do
      GetCharx;
end;}


procedure getstring;  {%%%%%%}
var i:integer;
begin
sasm:='                                                                                ';
i:=0;
getchar;
skipwhite;
while (look<>^M) do begin
i:=i+1;
sasm[i]:=upcase(look);
getcharX;
end;
getcharX;
end;

function recognize:boolean;
var
cap:boolean;
begin
cap:=false;
if sasm='FASM                                                                            '
then cap:=true;
recognize:=cap;
end;

procedure doasm;
begin {main}
getstring;
if not recognize then writeln(sasm);
while (not recognize) do begin
getstring;
if not recognize then writeln(sasm);
end;            {%%%%%}
next;
end;


{--------------------------------------------------------------}
{ Parse and Translate a Block of Statements }
procedure subdec;
begin
next;
scan;
if token <> 'x' then expected('sub name');
checkdup(value);
addentry(value,'S');
writeln(value+': ');
next; block;
matchstring('FSUB');
emitln('RET');

end;
procedure subrdec;
var s:string;
begin
s:=newlabel;
proc:=s;
next;
scan;
if token <> 'x' then expected('sub name');
checkdup(value);
addentry(value,'r');
writeln(value+': ');
writeln('SEGMENT .data public class=data align=1 ');
writeln(s+' dw 0');
writeln('SEGMENT .text public class=text align=1');
emitln('pop ax');
emitln('mov ['+s+'],ax');
next; block;
matchstring('FSUBR');
emitln('mov ax,['+s+']');
emitln('push ax');
emitln('RET');
proc:=' ';
end;
procedure doret;
var flag:boolean;
begin;
flag:=false;
if proc<>' ' then
begin
if look='(' then begin
next;
expression;
flag:=true;
end;
emitln('push eax');
emitln('mov ax,['+proc+']');
emitln('push ax');
emitln('RET');
end;
if not flag then next;
end;
procedure Block;
begin
   Scan;
   while not(Token in ['e', 'l']) do begin
      case Token of
       'X': doret;
       'a': doasm;
       'i': DoIf;
       'w': DoWhile;
       'q': Repeatun;
       'R': DoRead;
       'B': stampab;
       'W': DoWrite;
       'P': Per;
       'O': rep;
       'E': ter;
       'z': loop;
       'f': brek;
       else Assignment;
      end;
      Scan;
   end;
end;


{--------------------------------------------------------------}
{ Allocate Storage for a Variable }

procedure Alloc;
var tipo:char;
begin
   Next;
   (*questa parte di gestione variabili l'ho fatta tutta io..*)
   case KWcode[Lookup(Addr(KWlist), Value, NKW) + 1] of
   'h':begin tipo:='w'; Next;end;{*WORD(16 bittozzi)*}
   'j':begin tipo:='b'; Next;end;{*single byte(8 sporchi bit)*}
   'k':begin tipo:='d'; Next;end;{*double word(32 bit e crepi l'avarizia)*}
   else begin tipo:='w'; end;
   end;
   (*..*)
   if Token <> 'x' then Expected('Variable Name');
   CheckDup(Value);
   AddEntry(Value, tipo);
   Allocate(Value, '0',tipo);
   Next;
end;
procedure topdecls;forward;
procedure SubDecls;
begin
Scan;
while token in ['S','r'] do begin
case token of
'S':  begin
      subdec;
      scan;
      end;
'r': begin
     subrdec;
     scan;
     end;
end;
end;
end;
procedure TopDecls;
begin
   Scan;
   while Token = 'v' do
      Alloc;
      while Token = ',' do
         Alloc;
end;


function IsVarType(c: char): boolean;
begin
   IsVarType := c in ['b','w','d','B', 'W', 'D','r'];
end;
{ Get a Variable Type from the Symbol Table }

function VarType(Name: symbol): char;
var Typ: char;
begin
   Typ := Stype[locate(Name)];
   if not IsVarType(Typ) then Abort('Identifier ' + Name +
                                        ' is not a variable');
   VarType := Typ;
end;

{--------------------------------------------------------------}
{ Initialize }

{---------------------------------------------------------------}
{ Generate a Move Instruction }

procedure Move(Size: char; Dest:Symbol);
begin
case Size of
'b':begin
emitln('xor eax,eax');
emitln('mov al,['+Dest+']'); end;
'w':begin
emitln('xor eax,eax');
emitln('mov ax,['+Dest+']'); end;
'd':begin emitln('mov eax,['+Dest+']'); end;
'r':begin emitln('call '+Dest);
          emitln('pop eax');end;
end;
end;
{DA FARE}

procedure LoadVar(Name:Symbol);
begin
   if not InTable(Name) then Undefined(Name);
   Move(Vartype(Name),Name);
end;

{---------------------------------------------------------------}
{ Store Primary to Variable }

procedure StoreVar(Name:Symbol; Typ: char);
begin
case Stype[locate(Name)] of
'b':begin emitln('mov ['+Name+'],al'); end;
'w':begin emitln('mov ['+Name+'],ax'); end;
'd':begin emitln('mov ['+Name+'],eax'); end;
end;

end;


{--------------------------------------------------------------}
{ Store a Variable from the Primary Register }

procedure Store(Name: Symbol);
begin
   StoreVar(Name,VarType(Name));
end;


procedure Init;
VAR I:INTEGER;
begin
FOR I:=1 TO MAXENTRY do STYPE[I]:='?';
   GetChar;
   Next;
end;
(*Skippa un invio.*)
procedure fin;
begin
if look=CR then Begin
Getchar;
if look=LF then
Getchar;
end;
end;

{--------------------------------------------------------------}
{ Main Program }
procedure class;
begin
scan;
while token='I' do
begin
getname;
writeln('%include "c:\krakow\'+value+'.asm"');
if value='GRAPH' then begin
inc(farlab,11);
inc(lcount,11);
addentry('TMP','w');
addentry('R','w');
addentry('A','w');
addentry('B','w');
addentry('IX','w');
addentry('IY','w');
addentry('X','w');
addentry('X1','w');
addentry('Y','w');
addentry('Y1','w');
addentry('C','b');
addentry('VERDE','b');
addentry('ROSSO','b');
addentry('BLU','b');
addentry('GETPAL','S');
addentry('PAL','S');
addentry('SETMODEX','S');
addentry('RESTORE','S');
addentry('GETPIXEL','S');
addentry('PUTPIXEL','S');
addentry('CLS','S');
addentry('WAITRETRACE','S');
addentry('CERCHIO','S');
addentry('OLINE','S');
addentry('VLINE','S');
end;
IF VALUE='VIRTUAL' THEN BEGIN
inc(farlab,11);
inc(lcount,11);
addentry('VGETPIXEL','S');
addentry('VPUTPIXEL','S');
addentry('VCLS','S');
addentry('VCERCHIO','S');
addentry('VOLINE','S');
addentry('VVLINE','S');
addentry('FLIP','S');
addentry('VFLIP','S');
END;
if value='SOUND' then begin
addentry('FREQ','w');
addentry('COUNTER','w');
addentry('SOUND','S');
addentry('NOSOUND','S');
end;
if value='MISC' then begin
addentry('DELAY','S');
addentry('RANDOM','r');
end;
next;
scan;
end;
end;
begin
   iniz:=false;
   Init;
   MatchString('PROGRAM');
   class;
   Header;
   TopDecls;
   writeln('SEGMENT .text public class=code align=1');
   subDecls;
   MatchString('INIZIO');
   Prolog;
   Block;
   MatchString('FINE');
   Epilog;
end.
{--------------------------------------------------------------}
