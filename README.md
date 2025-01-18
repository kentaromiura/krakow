Krakow compiler
===

A little history:
===

In 2004 I wrote a compiler as a part of my high school final exam.

This might be one of my first open source contributions.

I decided to move the source to github from the original forum where I posted in 2004 before it disappear from there.

Bear in mind that it was 20 years ago and my sense of humour was very different back then.

At the time I release it under GPL 2, but with this I decided to re-license it under MIT.

Astonishingly enough it compile fine under current freepascal (3.2.2) when it was originally written for Turbo Pascal (6 or 7, can't remember the one we used at school).

What this compiler does is to translate a krakow file to a Nasm for DOS assembly, I assume the exact nasm version was 0.98.38 by looking at https://github.com/netwide-assembler/nasm/tags?after=nasm-0.99.02.

You'll need a linker, at the time MS Link was the only one working correctly for generating exe files, I think the version I posted doesn't return properly to DOS, I think I eventually fixed that but I never posted the updated version, probably because of losing my sources due to hard disk failure.


Why the name `krakow`
===
It was my last year of high school and we did a 3 days trip to krakow where I wrote a big part of the compiler on my portable 486, so I gave it that name.

This compiler followed the tutorial `LET'S BUILD A COMPILER! By Jack W. Crenshaw, Ph.D. I've found at the time at http://textfiles.com/programming/crenshawtut.txt, it's still up so you can follow yourself, that tutorial uses old motorola assembly code so I converted it to x86 myself.

I also included graphic asm from `To the VGA Trainer Program By DENTHOR of ASPHYXIA` also here: http://textfiles.com/programming/astrainer.txt

textfiles.com was invaluable as I didn't had internet at home and I could download all of it on some floppies

krakow is not a good language, in fact while it has a lot of features it misses things like string manipulations, because of that I see it as a high level macro generator for x86 assembly like C--, it's good enough though to generate a starfield demo, in amazing 320x200x256 under DOS as long as you understand italian and english (lol):
```
program
importa graph
importa misc
var i
inizio
setmodex
per(i=0:i<254:i=i+1)
    x=random
    delay
    c=random
    delay
    y=random
    delay
    PUTPIXEL
fper
leggi(tmp)
restore
fine.
```
In fact it looks like I've used it to generate the graph and sound libraries as they are equivalent to running the tip file I've posted and manually stripping the code from the `..start:` label.
It looks like I've manually made virtual by changing graph, I can tell by the different case.

How to use:
===
Files needs to be in the dos `CRLF` formats then you can 
`cat source.kra | ./main > output.asm`
it expects to have the .asm file libraries under c:\krakow, but you can manually change the `%include` directive in the generated assembly if you're so inclined.


After that you can pass it to a dos machine and compile via nasm and link the object file with microsoft link, which I don't know where to find but probably is included in https://winworldpc.com/product/microsoft-c-c/6x. I'm not aware of MS ever releasing ms link as freeware so you'd likely need a license (or a msdn account), it's also very possible that a Visual studio free version comes with a compatible linker, but I don't know if that' s the case.
