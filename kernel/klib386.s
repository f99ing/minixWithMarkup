# 
! sections

.sect .text; .sect .rom; .sect .data; .sect .bss

#include <minix/config.h>
#include <minix/const.h>
#include "const.h"
#include "sconst.h"
#include "protect.h"

! This file contains a number of assembly code utility routines needed by the
! kernel.  They are:

.define	_monitor	! exit Minix and return to the monitor
.define	_int86		! let the monitor make an 8086 interrupt call
.define	_cp_mess	! copies messages from source to destination
.define	_exit		! dummy for library routines
.define	__exit		! dummy for library routines
.define	___exit		! dummy for library routines
.define	___main		! dummy for GCC
.define	_phys_insw	! transfer data from (disk controller) port to memory
.define	_phys_insb	! likewise byte by byte
.define	_phys_outsw	! transfer data from memory to (disk controller) port
.define	_phys_outsb	! likewise byte by byte
.define	_enable_irq	! enable an irq at the 8259 controller
.define	_disable_irq	! disable an irq
.define	_phys_copy	! copy data from anywhere to anywhere in memory
.define	_phys_memset	! write pattern anywhere in memory
.define	_mem_rdw	! copy one word from [segment:offset]
.define	_reset		! reset the system
.define	_idle_task	! task executed when there is no work
.define	_level0		! call a function at level 0
.define	_read_tsc	! read the cycle counter (Pentium and up)
.define	_read_cpu_flags	! read the cpu flags

! The routines only guarantee to preserve the registers the C compiler
! expects to be preserved (ebx, esi, edi, ebp, esp, segment registers, and
! direction bit in the flags).

.sect .text
!*===========================================================================*
!*				monitor					     *
!*===========================================================================*
! PUBLIC void monitor();
! Return to the monitor.
!n _Monitor   makes it possible to return to the boot monitor. From the point of view of the boot monitor, all of MINIX 3 is just a subroutine, and when MINIX 3 is started, a return address to the monitor is left on the monitor's stack. _Monitor just has to restore the various segment selectors and the stack pointer that was saved when MINIX 3 was started, and then return as from any other subroutine.


_monitor:
	mov	esp, (_mon_sp)		! restore monitor stack pointer
    o16 mov	dx, SS_SELECTOR		! monitor data segment
	mov	ds, dx
	mov	es, dx
	mov	fs, dx
	mov	gs, dx
	mov	ss, dx
	pop	edi
	pop	esi
	pop	ebp
    o16 retf				! return to the monitor


!*===========================================================================*
!*				int86					     *
!*===========================================================================*
! PUBLIC void int86();
!n Int86 (line 8864) supports BIOS calls. The BIOS is used to provide alternative-disk drivers which are not described here. Int86 transfers control to the boot monitor, which manages a transfer from protected mode to real mode to execute a BIOS call, then back to protected mode for the return to 32-bit MINIX 3. The boot monitor also returns the number of clock ticks counted during the BIOS call. How this is used will be seen in the discussion of the clock task.


_int86:
	cmpb	(_mon_return), 0	! is the monitor there?
	jnz	0f
	movb	ah, 0x01		! an int 13 error seems appropriate
	movb	(_reg86+ 0), ah		! reg86.w.f = 1 (set carry flag)
	movb	(_reg86+13), ah		! reg86.b.ah = 0x01 = "invalid command"
	ret
0:	push	ebp			! save C registers
	push	esi
	push	edi
	push	ebx
	pushf				! save flags
	cli				! no interruptions

	inb	INT2_CTLMASK
	movb	ah, al
	inb	INT_CTLMASK
	push	eax			! save interrupt masks
	mov	eax, (_irq_use)		! map of in-use IRQ's
	and	eax, ~[1<<CLOCK_IRQ]	! keep the clock ticking
	outb	INT_CTLMASK		! enable all unused IRQ's and vv.
	movb	al, ah
	outb	INT2_CTLMASK

	mov	eax, SS_SELECTOR	! monitor data segment
	mov	ss, ax
	xchg	esp, (_mon_sp)		! switch stacks
	push	(_reg86+36)		! parameters used in INT call
	push	(_reg86+32)
	push	(_reg86+28)
	push	(_reg86+24)
	push	(_reg86+20)
	push	(_reg86+16)
	push	(_reg86+12)
	push	(_reg86+ 8)
	push	(_reg86+ 4)
	push	(_reg86+ 0)
	mov	ds, ax			! remaining data selectors
	mov	es, ax
	mov	fs, ax
	mov	gs, ax
	push	cs
	push	return			! kernel return address and selector
    o16	jmpf	20+2*4+10*4+2*4(esp)	! make the call
return:
	pop	(_reg86+ 0)
	pop	(_reg86+ 4)
	pop	(_reg86+ 8)
	pop	(_reg86+12)
	pop	(_reg86+16)
	pop	(_reg86+20)
	pop	(_reg86+24)
	pop	(_reg86+28)
	pop	(_reg86+32)
	pop	(_reg86+36)
	lgdt	(_gdt + GDT_SELECTOR )	! reload global descriptor table
	jmpf	CS_SELECTOR:csinit	! restore everything
csinit:	mov	eax, DS_SELECTOR
	mov	ds, ax
	mov	es, ax
	mov	fs, ax
	mov	gs, ax
	mov	ss, ax
	xchg	esp, (_mon_sp)		! unswitch stacks
	lidt	(_gdt+IDT_SELECTOR)	! reload interrupt descriptor table
	andb	(_gdt+TSS_SELECTOR+DESC_ACCESS), ~0x02  ! clear TSS busy bit
	mov	eax, TSS_SELECTOR
	ltr	ax			! set TSS register

	pop	eax
	outb	INT_CTLMASK		! restore interrupt masks
	movb	al, ah
	outb	INT2_CTLMASK

	add	(_lost_ticks), ecx	! record lost clock ticks

	popf				! restore flags
	pop	ebx			! restore C registers
	pop	edi
	pop	esi
	pop	ebp
	ret


!*===========================================================================*
!*				cp_mess					     *
!*===========================================================================*
! PUBLIC void cp_mess(int src, phys_clicks src_clicks, vir_bytes src_offset,
!		      phys_clicks dst_clicks, vir_bytes dst_offset);
! This routine makes a fast copy of a message from anywhere in the address
! space to anywhere else.  It also copies the source address provided as a
! parameter to the call into the first word of the destination message.
!
! Note that the message size, "Msize" is in DWORDS (not bytes) and must be set
! correctly.  Changing the definition of message in the type file and not
! changing it here will lead to total disaster.

!n used in CopyMess in proc.c
/*n Although _phys_copy (see below) could have been used for copying messages, _cp_mess (line 8952), a faster specialized procedure, has been provided for that purpose. It is called by

cp_mess(source, src_clicks, src_offset, dest_clicks, dest_offset);

 
where source is the sender's process number, which is copied into the m_source field of the receiver's buffer. Both the source and destination addresses are specified by giving a click number, typically the base of the segment containing the buffer, and an offset from that click. This form of specifying the source and destination is more efficient than the 32-bit addresses used by _phys_copy.
*/

CM_ARGS	=	4 + 4 + 4 + 4 + 4	! 4 + 4 + 4 + 4 + 4
!		es  ds edi esi eip	proc scl sof dcl dof

	.align	16
_cp_mess:
	cld
	push	esi
	push	edi
	push	ds
	push	es

	mov	eax, FLAT_DS_SELECTOR
	mov	ds, ax
	mov	es, ax

	mov	esi, CM_ARGS+4(esp)		! src clicks
	shl	esi, CLICK_SHIFT
	add	esi, CM_ARGS+4+4(esp)		! src offset
	mov	edi, CM_ARGS+4+4+4(esp)		! dst clicks
	shl	edi, CLICK_SHIFT
	add	edi, CM_ARGS+4+4+4+4(esp)	! dst offset

	mov	eax, CM_ARGS(esp)	! process number of sender
	stos				! copy number of sender to dest message
	add	esi, 4			! do not copy first word
	mov	ecx, Msize - 1		! remember, first word does not count
	rep
	movs				! copy the message

	pop	es
	pop	ds
	pop	edi
	pop	esi
	ret				! that is all folks!

/*n
_Exit,__exit, and ___exit (lines 9006 to 9008) are defined because some library routines that might be used in compiling MINIX 3 make calls to the standard C function exit. An exit from the kernel is not a meaningful concept; there is nowhere to go. Consequently, the standard exit cannot be used here. The solution here is to enable interrupts and enter an endless loop. Eventually, an I/O operation or the clock will cause an interrupt and normal system operation will resume. The entry point for ___main (line 9012) is another attempt to deal with a compiler action which, while it might make sense while compiling a user program, does not have any purpose in the kernel. It points to an assembly language ret (return from subroutine) instruction.


*/

!*===========================================================================*
!*				exit					     *
!*===========================================================================*
! PUBLIC void exit();
! Some library routines use exit, so provide a dummy version.
! Actual calls to exit cannot occur in the kernel.
! GNU CC likes to call ___main from main() for nonobvious reasons.

_exit:
__exit:
___exit:
	sti
	jmp	___exit

___main:
	ret

/*n
_Phys_insw (line 9022), _phys_insb (line 9047), _phys_outsw (line 9072), and _phys_outsb (line 9098), provide access to I/O ports, which on Intel hardware occupy a separate address space from memory and use different instructions from memory reads and writes. The I/O instructions used here, ins, insb, outs, and outsb, are designed to work efficiently with arrays (strings), and either 16-bit words or 8-bit bytes. The additional instructions in each function set up all the parameters needed to move a given number of bytes or words between a buffer, addressed physically, and a port. This method provides the speed needed to service disks, which must be serviced more rapidly than could be done with simpler byte- or word-at-a-time I/O operations.


*/
!*===========================================================================*
!*				phys_insw				     *
!*===========================================================================*
! PUBLIC void phys_insw(Port_t port, phys_bytes buf, size_t count);
! Input an array from an I/O port.  Absolute address version of insw().
!* ub: 
_phys_insw:
	push	ebp
	mov	ebp, esp
	cld				!n CLD: Clear Direction Flag。 When the DF flag is set to 0, string operations increment the index registers (ESI and/or EDI).
	push	edi
	push	es
	mov	ecx, FLAT_DS_SELECTOR
	mov	es, cx
	mov	edx, 8(ebp)		! port to read from
	mov	edi, 12(ebp)		! destination addr
	mov	ecx, 16(ebp)		! byte count
	shr	ecx, 1			! word count		!n shift ecx right 1 bit. shift logical right (SHR) instructions shift the bits of the destination operand to the right,For each shift count, the least significant bit of the destination operand is shifted into the CF flag,  the most significant bit is cleared. 
rep o16	ins				! input many words   !n ins: input string from port
	pop	es
	pop	edi
	pop	ebp
	ret


!*===========================================================================*
!*				phys_insb				     *
!*===========================================================================*
! PUBLIC void phys_insb(Port_t port, phys_bytes buf, size_t count);
! Input an array from an I/O port.  Absolute address version of insb().

_phys_insb:
	push	ebp
	mov	ebp, esp
	cld
	push	edi
	push	es
	mov	ecx, FLAT_DS_SELECTOR
	mov	es, cx
	mov	edx, 8(ebp)		! port to read from
	mov	edi, 12(ebp)		! destination addr
	mov	ecx, 16(ebp)		! byte count
!	shr	ecx, 1			! word count
   rep	insb				! input many bytes
	pop	es
	pop	edi
	pop	ebp
	ret


!*===========================================================================*
!*				phys_outsw				     *
!*===========================================================================*
! PUBLIC void phys_outsw(Port_t port, phys_bytes buf, size_t count);
! Output an array to an I/O port.  Absolute address version of outsw().

	.align	16
_phys_outsw:
	push	ebp
	mov	ebp, esp
	cld
	push	esi
	push	ds
	mov	ecx, FLAT_DS_SELECTOR
	mov	ds, cx
	mov	edx, 8(ebp)		! port to write to
	mov	esi, 12(ebp)		! source addr
	mov	ecx, 16(ebp)		! byte count
	shr	ecx, 1			! word count
rep o16	outs				! output many words
	pop	ds
	pop	esi
	pop	ebp
	ret


!*===========================================================================*
!*				phys_outsb				     *
!*===========================================================================*
! PUBLIC void phys_outsb(Port_t port, phys_bytes buf, size_t count);
! Output an array to an I/O port.  Absolute address version of outsb().

	.align	16
_phys_outsb:
	push	ebp
	mov	ebp, esp
	cld
	push	esi
	push	ds
	mov	ecx, FLAT_DS_SELECTOR
	mov	ds, cx
	mov	edx, 8(ebp)		! port to write to
	mov	esi, 12(ebp)		! source addr
	mov	ecx, 16(ebp)		! byte count
   rep	outsb				! output many bytes
	pop	ds
	pop	esi
	pop	ebp
	ret

!n A single machine instruction can enable or disable the CPU's response to all interrupts. _Enable_irq (line 9126) and _disable_irq (line 9162) are more complicated. They work at the level of the interrupt controller chips to enable and disable individual hardware interrupts.
!n exp usage :  init_clock() in clock.c;
!*==========================================================================*
!*				enable_irq				    *
!*==========================================================================*/
! PUBLIC void enable_irq(irq_hook_t *hook)
! Enable an interrupt request line by clearing an 8259 bit.
! Equivalent C code for hook->irq < 8:
!   if ((irq_actids[hook->irq] &= ~hook->id) == 0)
!	outb(INT_CTLMASK, inb(INT_CTLMASK) & ~(1 << irq));

	.align	16
_enable_irq:
	push	ebp
	mov	ebp, esp
	pushf
	cli
	mov	eax, 8(ebp)		! hook
	mov	ecx, 8(eax)		! irq
	mov	eax, 12(eax)		! id bit
	not	eax
	and	_irq_actids(ecx*4), eax	! clear this id bit
	jnz	en_done			! still masked by other handlers?
	movb	ah, ~1
	rolb	ah, cl			! ah = ~(1 << (irq % 8))
	mov	edx, INT_CTLMASK	! enable irq < 8 at the master 8259
	cmpb	cl, 8
	jb	0f
	mov	edx, INT2_CTLMASK	! enable irq >= 8 at the slave 8259
0:	inb	dx
	andb	al, ah
	outb	dx			! clear bit at the 8259
en_done:popf
	leave
	ret


!*==========================================================================*
!*				disable_irq				    *
!*==========================================================================*/
! PUBLIC int disable_irq(irq_hook_t *hook)
! Disable an interrupt request line by setting an 8259 bit.
! Equivalent C code for irq < 8:
!   irq_actids[hook->irq] |= hook->id;
!   outb(INT_CTLMASK, inb(INT_CTLMASK) | (1 << irq));
! Returns true iff the interrupt was not already disabled.

	.align	16
_disable_irq:
	push	ebp
	mov	ebp, esp
	pushf
	cli
	mov	eax, 8(ebp)		! hook
	mov	ecx, 8(eax)		! irq
	mov	eax, 12(eax)		! id bit
	or	_irq_actids(ecx*4), eax	! set this id bit
	movb	ah, 1
	rolb	ah, cl			! ah = (1 << (irq % 8))
	mov	edx, INT_CTLMASK	! disable irq < 8 at the master 8259
	cmpb	cl, 8
	jb	0f
	mov	edx, INT2_CTLMASK	! disable irq >= 8 at the slave 8259
0:	inb	dx
	testb	al, ah
	jnz	dis_already		! already disabled?
	orb	al, ah
	outb	dx			! set bit at the 8259
	mov	eax, 1			! disabled by this function
	popf
	leave
	ret
dis_already:
	xor	eax, eax		! already disabled
	popf
	leave
	ret

/*n
_Phys_copy (line 9204) is called in C by

phys_copy(source_address, destination_address, bytes);


and copies a block of data from anywhere in physical memory to anywhere else. Both addresses are absolute, that is, address 0 really means the first byte in the entire address space, and all three parameters are unsigned longs.
*/

!*===========================================================================*
!*				phys_copy				     *
!*===========================================================================*
! PUBLIC void phys_copy(phys_bytes source, phys_bytes destination,
!			phys_bytes bytecount);
! Copy a block of physical memory.

PC_ARGS	=	4 + 4 + 4 + 4	! 4 + 4 + 4
!		es edi esi eip	 src dst len

	.align	16
_phys_copy:
	cld
	push	esi
	push	edi
	push	es

	mov	eax, FLAT_DS_SELECTOR
	mov	es, ax

	mov	esi, PC_ARGS(esp)
	mov	edi, PC_ARGS+4(esp)
	mov	eax, PC_ARGS+4+4(esp)

	cmp	eax, 10			! avoid align overhead for small counts
	jb	pc_small
	mov	ecx, esi		! align source, hope target is too
	neg	ecx
	and	ecx, 3			! count for alignment
	sub	eax, ecx
	rep
   eseg	movsb
	mov	ecx, eax
	shr	ecx, 2			! count of dwords
	rep
   eseg	movs
	and	eax, 3
pc_small:
	xchg	ecx, eax		! remainder
	rep
   eseg	movsb

	pop	es
	pop	edi
	pop	esi
	ret
!n For security, all memory to be used by a program should be wiped clean of any data remaining from a program that previously occupied that memory. This is done by the MINIX 3 exec call, ultimately using the next function in klib386.s, phys_memset
!*===========================================================================*
!*				phys_memset				     *
!*===========================================================================*
! PUBLIC void phys_memset(phys_bytes source, unsigned long pattern,
!	phys_bytes bytecount);
! Fill a block of physical memory with pattern.

	.align	16
_phys_memset:
	push	ebp
	mov	ebp, esp
	push	esi
	push	ebx
	push	ds
	mov	esi, 8(ebp)
	mov	eax, 16(ebp)
	mov	ebx, FLAT_DS_SELECTOR
	mov	ds, bx
	mov	ebx, 12(ebp)
	shr	eax, 2
fill_start:
   	mov     (esi), ebx
	add	esi, 4
	dec	eax
	jnz	fill_start
	! Any remaining bytes?
	mov	eax, 16(ebp)
	and	eax, 3
remain_fill:
	cmp	eax, 0
	jz	fill_done
	movb	bl, 12(ebp)
   	movb    (esi), bl
	add	esi, 1
	inc	ebp
	dec	eax
	jmp	remain_fill
fill_done:
	pop	ds
	pop	ebx
	pop	esi
	pop	ebp
	ret

!n _Mem_rdw (line 9291) returns a 16-bit word from anywhere in memory. The result is zero-extended into the 32-bit eax register.  specific to Intel processors
!*===========================================================================*
!*				mem_rdw					     *
!*===========================================================================*
! PUBLIC u16_t mem_rdw(U16_t segment, u16_t *offset);
! Load and return word at far pointer segment:offset.

	.align	16
_mem_rdw:
	mov	cx, ds
	mov	ds, 4(esp)		! segment
	mov	eax, 4+4(esp)		! offset
	movzx	eax, (eax)		! word to return
	mov	ds, cx
	ret

!n specific to Intel processors.  The _reset function (line 9307) resets the processor. It does this by loading the processor's interrupt descriptor table register with a null pointer and then executing a software interrupt. This has the same effect as a hardware reset.
!*===========================================================================*
!*				reset					     *
!*===========================================================================*
! PUBLIC void reset();
! Reset the system by loading IDT with offset 0 and interrupting.

_reset:
	lidt	(idt_zero)
	int	3		! anything goes, the 386 will not like it
.sect .data
idt_zero:	.data4	0, 0
.sect .text

!n The idle_task (line 9318) is called when there is nothing else to do. It is written-as an endless loop, but it is not just a busy loop (which could have been used to have the same effect). Idle_task takes advantage of the availability of a hlt instruction, which puts the processor into a power-conserving mode until an interrupt is received. However, hlt is a privileged instruction and executing hlt when the current privilege level is not 0 will cause an exception. So idle_task pushes the address of a subroutine containing a hlt and then calls level0 (line 9322). This function retrieves the address of the halt subroutine, and copies it to a reserved storage area (declared in glo.h and actually reserved in table.c).
!*===========================================================================*
!*				idle_task				     *
!*===========================================================================*
_idle_task:
! This task is called when the system has nothing else to do.  The HLT
! instruction puts the processor in a state where it draws minimum power.
	push	halt
	call	_level0		! level0(halt)
	pop	eax
	jmp	_idle_task
halt:
	sti
	hlt
	cli
	ret

!n _Level0 treats whatever address is preloaded to this area as the functional part of an interrupt service routine to be run with the most privileged permission level, level zero.
!*===========================================================================*
!*			      level0					     *
!*===========================================================================*
! PUBLIC void level0(void (*func)(void))
! Call a function at permission level 0.  This allows kernel tasks to do
! things that are only possible at the most privileged CPU level.
!
_level0:
	mov	eax, 4(esp)
	mov	(_level0_func), eax
	int	LEVEL0_VECTOR
	ret

!n   reads a CPU register which executes an assembly language instruction known as rdtsc, read time stamp counter. This counts CPU cycles and is intended for benchmarking or debugging. This instruction is not supported by the MINIX 3 assembler, and is generated by coding the opcode in hexadecimal. 
!*===========================================================================*
!*			      read_tsc					     *
!*===========================================================================*
! PUBLIC void read_tsc(unsigned long *high, unsigned long *low);
! Read the cycle counter of the CPU. Pentium and up. 
.align 16
_read_tsc:
.data1 0x0f		! this is the RDTSC instruction 
.data1 0x31		! it places the TSC in EDX:EAX
	push ebp
	mov ebp, 8(esp)
	mov (ebp), edx
	mov ebp, 12(esp)
	mov (ebp), eax
	pop ebp
	ret

!n reads the processor flags and returns them as a C variable. The programmer was tired and the comment about the purpose of this function is incorrect.
!*===========================================================================*
!*			      read_flags					     *
!*===========================================================================*
! PUBLIC unsigned long read_cpu_flags(void);
! Read CPU status flags from C.
.align 16
_read_cpu_flags:
	pushf
	mov eax, (esp)
	popf
	ret

