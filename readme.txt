
feature view

user view

file view

concetptual structure view

user view:


asm用某个assembler，需要了解其语法和用法。
有个中国人学习过程中写了篇有关其语法的文章，之前下载了。
is this?   :THE MINIX ASSEMBLY LANGUAGE MANUAL http://minix1.woodhull.com/faq/MinixAsMn.html

 The general strategy is to do as much as possible using high-level C code. As we have seen, there are already two versions of the mpx code. One chunk of C code can eliminate two chunks of assembler code.


3.1.0 is the version the book used.

book version is usable only on 32-bit machines with 80386 or better processors. It does not work in 16-bit mode, and creation of a 16-bit version may require removing some features.
Nevertheless, a common base of C source code is used and the compiler generates the appropriate output depending upon whether the compiler itself is the 16-bit or 32-bit version of the compiler. A macro defined by the compiler itself determines the definition of the _WORD_SIZE macro in the file include/minix/sys_config.h.

entry point for the MINIX kernel is in kernel/mpx386.s or kernel/mpx86.s,selected by kernel/mpx.s
different source code files must be used for the 16-bit or 32-bit compiler. 
The 32-bit version of the initialization code is in mpx386.s. The alternative, for 16-bit systems, is in mpx88.s.
Both of these also include assembly language support for other low-level kernel operations.

Once the bootstrap process has loaded the operating system into memory, control is transferred to the label MINIX (in mpx386.s

boot
.boot monitor,installBoot,boot sector code
.installBoot put these code to hard disk/floppy ?
.masterboot.s is the first boot to execute ?
.bootimage.c  bootminix(void) contains code that load minix and run it

try:form a model of minix
kernel ,system task,clock task are build into one image but run in different process.
pm :process manager
fs:
drivers

--lib/

lib\i386\rts\_ipc.s
seems to be IPC code:send receive ...


--kernel/
kernel/system.c
system call entry,run as a loop,dispatch to functions in kernel/system

kernel/system
see kernel/system.c

main.c
contains main(), before this to run,enviroment are set by mpx.s


the kernel has a library of support functions written in assembly language that are included by compiling klib.s and a few utility programs, written in C, in the file misc.c( utility.c?).
Klib.s  is a short file similar to mpx.s, which selects the appropriate machine-specific version based upon the definition of WORD_SIZE.

utility.c

!n	all functions compiled by the C compiler have an underscore prepended to their names in the symbol tables, and the linker looks for such names when separately compiled modules are linked.
exp like cstart()
