!sections: 
! comment , asm section define, include, #define TSS3_S_SP0, define exported functions, define exported vars
! flags,system init code(including call _cstart, end with jmp _main).
! interrupt handlers, save, _s_call, restart, exception related, level0_call, data

# 
! This file, mpx386.s, is included by mpx.s when Minix is compiled for 
! 32-bit Intel CPUs. The alternative mpx88.s is compiled for 16-bit CPUs.

! This file is part of the lowest layer of the MINIX kernel.  (The other part
! is "proc.c".)  The lowest layer does process switching and message handling.
! Furthermore it contains the assembler startup code for Minix and the 32-bit
! interrupt handlers.  It cooperates with the code in "start.c" to set up a 
! good environment for main().

!n The part of the comment on lines 6310 to 6315 should say that a kernel reentry can occur only when an exception is detected.  book p174

! Every transition to the kernel goes through this file.  Transitions to the 
! kernel may be nested.  The initial entry may be with a system call (i.e., 
! send or receive a message), an exception or a hardware interrupt;  kernel 
! reentries may only be made by hardware interrupts.  The count of reentries 
! is kept in "k_reenter". It is important for deciding whether to switch to 
! the kernel stack and for protecting the message passing code in "proc.c".

! For the message passing trap, most of the machine state is saved in the
! proc table.  (Some of the registers need not be saved.)  Then the stack is
! switched to "k_stack", and interrupts are reenabled.  Finally, the system
! call handler (in C) is called.  When it returns, interrupts are disabled
! again and the code falls into the restart routine, to finish off held-up
! interrupts and run the process or task whose pointer is in "proc_ptr".

! Hardware interrupt handlers do the same, except  (1) The entire state must
! be saved.  (2) There are too many handlers to do this inline, so the save
! routine is called.  A few cycles are saved by pushing the address of the
! appropiate restart routine for a return later.  (3) A stack switch is
! avoided when the stack is already switched.  (4) The (master) 8259 interrupt
! controller is reenabled centrally in save().  (5) Each interrupt handler
! masks its interrupt line using the 8259 before enabling (other unmasked)
! interrupts, and unmasks it after servicing the interrupt.  This limits the
! nest level to the number of lines and protects the handler from itself.

! For communication with the boot monitor at startup time some constant
! data are compiled into the beginning of the text segment. This facilitates 
! reading the data at the start of the boot process, since only the first
! sector of the file needs to be read.

! Some data storage is also allocated at the end of this file. This data 
! will be at the start of the data segment of the kernel and will be read
! and modified by the boot monitor before the kernel starts.

! sections

.sect .text
begtext:
.sect .rom
begrom:
.sect .data
begdata:
.sect .bss
begbss:

#include <minix/config.h>
#include <minix/const.h>
#include <minix/com.h>
#include <ibm/interrupt.h>
#include "const.h"
#include "protect.h"
#include "sconst.h"

/* Selected 386 tss offsets. */
#define TSS3_S_SP0	4

! Exported functions
! Note: in assembly language the .define statement applied to a function name 
! is loosely equivalent to a prototype in C code -- it makes it possible to
! link to an entity declared in the assembly code but does not create
! the entity.

.define	_restart
.define	save

.define	_divide_error
.define	_single_step_exception
.define	_nmi
.define	_breakpoint_exception
.define	_overflow
.define	_bounds_check
.define	_inval_opcode
.define	_copr_not_available
.define	_double_fault
.define	_copr_seg_overrun
.define	_inval_tss
.define	_segment_not_present
.define	_stack_exception
.define	_general_protection
.define	_page_fault
.define	_copr_error

.define	_hwint00	! handlers for hardware interrupts
.define	_hwint01
.define	_hwint02
.define	_hwint03
.define	_hwint04
.define	_hwint05
.define	_hwint06
.define	_hwint07
.define	_hwint08
.define	_hwint09
.define	_hwint10
.define	_hwint11
.define	_hwint12
.define	_hwint13
.define	_hwint14
.define	_hwint15

.define	_s_call
.define	_p_s_call
.define	_level0_call

! Exported variables.
.define	begbss
.define	begdata

.sect .text


!*===========================================================================*
!*				MINIX					     *
!*===========================================================================*

!The boot monitor always starts in 16-bit mode, but switches the CPU to 32-bit mode if necessary. This happens before control passes to the label MINIX.

MINIX:				! this is the entry point for the MINIX kernel
	jmp	over_flags	! skip over the next few bytes
	.data2	CLICK_SHIFT	! for the monitor: memory granularity
flags:
!At this point the flags have already served their purpose; they were read by the monitor when it loaded the kernel into memory. 
!They are located here because it is an easily specified address. They are used by the boot monitor to identify various characteristics of the kernel, most importantly, whether it is a 16-bit or 32-bit system.
	.data2	0x01FD		! boot monitor flags:
				!	call in 386 mode, make bss, make stack,
				!	load high, don't patch, will return,
				!	uses generic INT, memory vector,
				!	new boot code return

	nop			! extra byte to sync up disassembler

over_flags:

! Set up a C stack frame on the monitor stack.  (The monitor sets cs and ds
! right.  The ss descriptor still references the monitor data segment.)

	movzx	esp, sp		! monitor stack is a 16 bit stack  !movzx :move with zero extended,for example,Move byte to word with zero-extension,or byte to double word,or word to double word.
	push	ebp
	mov	ebp, esp	!move to ebp so it can be used with offsets to retrieve from the stack the values placed there by the monitor,see 'sec boot param' below
	push	esi
	push	edi
	cmp	4(ebp), 0	! monitor return vector is
	jz	noret		! nonzero if return possible
	inc	(_mon_return)	!n defined in c in boot/boot.h ,an int var. set in cstart
noret:	mov	(_mon_sp), esp	! save stack pointer for later return !_mon_sp:monitor stack,defined in c in kernel/glo.h,of type unsigned .

! Copy the monitor global descriptor table to the address space of kernel and
! switch over to it.  Prot_init() can then update it with immediate effect.

	sgdt	(_gdt+GDT_SELECTOR)		! get the monitor gdtr !n gdt is defined in protect.c as :segdesc_s gdt[]
	mov	esi, (_gdt+GDT_SELECTOR+2)	! absolute address of GDT
	mov	ebx, _gdt			! address of kernel GDT
	mov	ecx, 8*8			! copying eight descriptors
copygdt:
 eseg	movb	al, (esi)
	movb	(ebx), al
	inc	esi
	inc	ebx
	loop	copygdt
	mov	eax, (_gdt+DS_SELECTOR+2)	! base of kernel data
	and	eax, 0x00FFFFFF			! only 24 bits
	add	eax, _gdt			! eax = vir2phys(gdt)
	mov	(_gdt+GDT_SELECTOR+2), eax	! set base of GDT
	lgdt	(_gdt+GDT_SELECTOR)		! switch over to kernel GDT

!sec boot param
! retrieve from the stack the values placed there by the monitor,ebp is sp set by monitor ,as in the 3 instructions after over_flags above.
!because the stack grows downward with Intel processors, 8(ebp) refers to a value pushed subsequent to pushing the value located at 12(ebp).
/*n
The monitor passes several parameters to MINIX 3, by putting them on the stack:
..First the monitor pushes the address of the variable aout, which holds the address of an array of the header information of the component programs of the boot image. 
..Next it pushes the size and then the address of the boot parameters. These are all 32-bit quantities. 
..Next come the monitor's code segment address and the location to return to within the monitor when MINIX 3 terminates. These are both 16-bit quantities, since the monitor operates in 16-bit protected mode. 
the stack looks like this:
16(ebp)  aout
12(ebp)  size  of the boot parameters
8(ebp)	address of the boot parameters
	 monitor's code segment address and the location to return to within the monitor when MINIX 3 terminates
ebp 
*/
! Locate boot parameters, set up kernel segment registers and stack.
	mov	ebx, 8(ebp)	! boot parameters offset
	mov	edx, 12(ebp)	! boot parameters length
	mov	eax, 16(ebp)	! address of a.out headers
	mov	(_aout), eax	!n aout is of c phys_bytes( unsigned long) type
	mov	ax, ds		! kernel data
	mov	es, ax
	mov	fs, ax
	mov	gs, ax
	mov	ss, ax
	mov	esp, k_stktop	! set sp to point to the top of kernel stack
!n  ?before call cstart,the asm code must set up a stack frame to provide the proper environment for code compiled by the C compiler, copying tables used by the processor to define memory segments, and setting up various processor registers

! Call C startup code to set up a proper environment to run main().
!n first push the 5 params needed by cstart. in reverse order
	push	edx		!n parmsize
	push	ebx		!n parmoff
	push	SS_SELECTOR	!n mds
	push	DS_SELECTOR	!n ds
	push	CS_SELECTOR	!n cs

	call	_cstart		! cstart(cs, ds, mds, parmoff, parmlen). in start.c
	 
	add	esp, 5*4

	/*n 
	Reload gdtr, idtr and the segment registers to global descriptor table set up by prot_init(),which is called by cstart()

	prot_init() initialize the Global Descriptor Table, the central data structure used by Intel 32-bit processors to oversee memory protection, 
	and the Interrupt Descriptor Table, used to select the code to be executed for each possible interrupt type.
	the lgdt and lidt instructions make these tables effective by loading the registers which contains the address of these tables.
	*/
	lgdt	(_gdt+GDT_SELECTOR)
	lidt	(_gdt+IDT_SELECTOR)

	jmpf	CS_SELECTOR:csinit	!n  jmpf indicate far jump. syntax : jmpf SEGMENT:OFFSET   ref [1]
	! This jump forces use of the structures just initialized.  

	!n following to _main are not explained 
csinit:		
    o16	mov	ax, DS_SELECTOR
	mov	ds, ax
	mov	es, ax
	mov	fs, ax
	mov	gs, ax
	mov	ss, ax
    o16	mov	ax, TSS_SELECTOR	! no other TSS is used
	ltr	ax
	push	0			! set flags to known good state
	popf				! esp, clear nested task and int enable

	jmp	_main			! main()    !n at this point the initialization code  is complete. 

!*===========================================================================*
!*				interrupt handlers			     *
!*		interrupt handlers for 386 32-bit protected mode	     *
!*===========================================================================*

/*n
 Irq_handlers (tables defined in glo.h) contains the hook information, including addresses of handler routines. The number of the interrupt being serviced is converted to an address within  irq_handlers. This address is then pushed onto the stack as the argument to _intr_handle, and _intr_handle is called

not only does it(intr_handle) call the service routine for the interrupt that was called, it sets or resets a flag in the _irq_actids array to indicate whether this attempt to service the interrupt succeeded, and it gives other entries on the queue another chance to run and be removed from the list. 

Depending upon exactly what was required of the handler, the IRQ may or may not be available to receive another interrupt upon the return from the call to _intr_handle. This is determined by checking the corresponding entry in _irq_actids.

*/
/*n
	A nonzero value in _irq_actids shows that interrupt service for this IRQ is not complete. If so, the interrupt controller is manipulated to prevent it from responding to another interrupt from the same IRQ line.This operation masks the ability of the controller chip to respond to a particular input; the CPU's ability to respond to all interrupts is inhibited internally when it first receives the interrupt signal and has not yet been restored at this point.
	  
	An element of the  irq_actids array is a bitmap that records the results for all the handlers on the list in such a way that the result will be zero if and only if every one of the handlers returned TRUE. If that is not the case, the code on following 3 lines  disables the IRQ before the interrupt controller as a whole is reenabled on line _hwint01.

	This mechanism ensures that none of the handlers on the chain belonging to an IRQ will be activated until all of the device drivers to which these handlers belong have completed their work. Obviously, there needs to be another way to reenable an IRQ. That is provided in a function enable_irq

	Suffice it to say, each device driver must be sure that enable_irq is called when its work is done. It also is obvious that enable_irq first should reset its own bit in the element of _irq_act_ids that corresponds to the IRQ of the driver, and then should test whether all bits have been reset. Only then should the IRQ be reenabled on the interrupt controller chip.

	What we have just described applies in its simplest form only to the clock driver, because the clock is the only interrupt-driven device that is compiled into the kernel binary. The address of an interrupt handler in another process is not meaningful in the context of the kernel, and the enable_irq function in the kernel cannot be called by a separate process in its own memory space. For user-space device drivers, which means all device drivers that respond to hardware-initiated interrupts except for the clock driver, the address of a common handler, generic_handler, is stored in the linked list of hooks.

	The other information in each element of the list of hooks includes the process number of the associated device driver. When generic_handler is called it sends a message to the correct device driver which causes the specific handler functions of the driver to run. The system task supports the other end of the chain of events described above as well. When a user-space device driver completes its work it makes a sys_irqctl kernel call, which causes the system task to call enable_irq on behalf of that driver to prepare for the next interrupt.

	*/
!*===========================================================================*
!*				hwint00 - 07				     *
!*===========================================================================*
! Note this is a macro, it just looks like a subroutine.
!n the reason for using macro instead of call:  In servicing an interrupt, speed is important, and doing it this way eliminates the overhead of executing code to load a parameter, call a subroutine, and retrieve the parameter.

!n An entry point( _hwint00 - _hwint15) exists for each interrupt, these are registered in 
!n before hwint_master begins to execute, the CPU has created a new stack in the stackframe_s of the interrupted process, within its process table slot. Several key registers have already been saved there, and all interrupts are disabled. 
!n 

#define hwint_master(irq)	\
	call	save			/* save interrupted process state */;\  !n  Upon returning to hwint_master, the kernel stack, not a stackframe in the process table, is in use.

	push	(_irq_handlers+4*irq)	/* irq_handlers[irq]		  */;\
	call	_intr_handle		/*n intr_handle(irq_handlers[irq]) in i8259.c */;\  
	pop	ecx							    ;\
	
	cmp	(_irq_actids+4*irq), 0	/* interrupt still active?	  */;\
	jz	0f							    ;\
	!n The 0f is not a hexadecimal number, nor is it a normal label. Ordinary label names are not permitted to begin with numeric characters. This is the way the MINIX 3 assembler specifies a local label; the 0f means a jump forward to the next numeric label 0.
	!n when resolving a local label, the assembler uses the nearest one that matches in the specified direction, and additional occurrences of a local label are ignored.


	inb	INT_CTLMASK		/* get current mask */		    ;\
	orb	al, [1<<irq]		/* mask irq */			    ;\
	outb	INT_CTLMASK		/* disable the irq		  */;\
0:	movb	al, END_OF_INT						    ;\
	outb	INT_CTL			/* reenable master 8259		  */;\!n allows the interrupt controller to resume normal operation, possibly with the line for the current interrupt disabled.

	ret				/* restart (another) process      */
/*n
If a process has been interrupted, the stack in use at this point is the kernel stack, and not the stack within a process table that was set up by the hardware before hwint_master was started. In this case, manipulation of the stack by save will have left the address of _restart on the kernel stack. This results in a task, driver, server, or user process once again executing. It may not be, and in fact very likely is not, the same process as was executing when the interrupt occurred. This depends upon whether the processing of the message created by the device-specific interrupt service routine caused a change in the process scheduling queues. In the case of a hardware interrupt this will almost always be the case. Interrupt handlers usually result in messages to device drivers, and device drivers generally are queued on higher priority queues than user processes.
*/
/*n
To be complete, let us mention that if an interrupt could occur while kernel code were executing, the kernel stack would already be in use, and save would leave the address of restart1 on the kernel stack. In this case, whatever the kernel was doing previously would continue after the ret at the end of hwint_master. This is a description of handling of nested interrupts, and these are not allowed to occur in MINIX 3 interrupts are not enabled while kernel code is running. However, as mentioned previously, the mechanism is necessary in order to handle exceptions. When all the kernel routines involved in responding to an exception are complete_restart will finally execute. In response to an exception while executing kernel code it will almost certainly be true that a process different from the one that was interrupted last will be put into execution. The response to an exception in the kernel is a panic, and what happens will be an attempt to shut down the system with as little damage as possible.

*/
/*n

interrupt
include\unistd.h lists system calls,goes to _syscall-- __sendrec ,_sendrec invoke SYSVEC interrupt ,SYSVEC = SYS386_VECTOR/SYS_VECTOR

*/
!n hwint01-15  are added in prot_init

! Each of these entry points is an expansion of the hwint_master macro
	.align	16
_hwint00:		! Interrupt routine for irq 0 (the clock).
	hwint_master(0)

	.align	16
_hwint01:		! Interrupt routine for irq 1 (keyboard)
	hwint_master(1)

	.align	16
_hwint02:		! Interrupt routine for irq 2 (cascade!)
	hwint_master(2)

	.align	16
_hwint03:		! Interrupt routine for irq 3 (second serial)
	hwint_master(3)

	.align	16
_hwint04:		! Interrupt routine for irq 4 (first serial)
	hwint_master(4)

	.align	16
_hwint05:		! Interrupt routine for irq 5 (XT winchester)
	hwint_master(5)

	.align	16
_hwint06:		! Interrupt routine for irq 6 (floppy)
	hwint_master(6)

	.align	16
_hwint07:		! Interrupt routine for irq 7 (printer)
	hwint_master(7)

!*===========================================================================*
!*				hwint08 - 15				     *
!*===========================================================================*
! Note this is a macro, it just looks like a subroutine.
!n Hwint_slave (line 6566) is similar to hwint_master, except that it must reenable both the master and slave controllers, since both of them are disabled by receipt of an interrupt by the slave.


#define hwint_slave(irq)	\
	call	save			/* save interrupted process state */;\   
	push	(_irq_handlers+4*irq)	/* irq_handlers[irq]		  */;\
	call	_intr_handle		/* intr_handle(irq_handlers[irq]) */;\
	pop	ecx							    ;\
	cmp	(_irq_actids+4*irq), 0	/* interrupt still active?	  */;\
	jz	0f							    ;\
	inb	INT2_CTLMASK						    ;\
	orb	al, [1<<[irq-8]]					    ;\
	outb	INT2_CTLMASK		/* disable the irq		  */;\
0:	movb	al, END_OF_INT						    ;\
	outb	INT_CTL			/* reenable master 8259		  */;\
	outb	INT2_CTL		/* reenable slave 8259		  */;\
	ret				/* restart (another) process      */

! Each of these entry points is an expansion of the hwint_slave macro
	.align	16
_hwint08:		! Interrupt routine for irq 8 (realtime clock)
	hwint_slave(8)

	.align	16
_hwint09:		! Interrupt routine for irq 9 (irq 2 redirected)
	hwint_slave(9)

	.align	16
_hwint10:		! Interrupt routine for irq 10
	hwint_slave(10)

	.align	16
_hwint11:		! Interrupt routine for irq 11
	hwint_slave(11)

	.align	16
_hwint12:		! Interrupt routine for irq 12
	hwint_slave(12)

	.align	16
_hwint13:		! Interrupt routine for irq 13 (FPU exception)
	hwint_slave(13)

	.align	16
_hwint14:		! Interrupt routine for irq 14 (AT winchester)
	hwint_slave(14)

	.align	16
_hwint15:		! Interrupt routine for irq 15
	hwint_slave(15)

!*===========================================================================*
!*				save					     *
!*===========================================================================*
! Save for protected mode.
! This is much simpler than for 8086 mode, because the stack already points
! into the process table, or has already been switched to the kernel stack.
!n  This subroutine pushes all the other registers necessary to restart the interrupted process. Save could have been written inline as part of the macro to increase speed, but this would have more than doubled the size of the macro, and in any case save is needed for calls by other functions.
/*n
one of its functions is to save the context of the interrupted process on the stack provided by the CPU, which is a stackframe within the process table.

ub hwint_master
*/
	.align	16
save:
	cld			! set direction flag to a known value
	pushad			! save "general" registers
    o16	push	ds		! save ds
    o16	push	es		! save es
    o16	push	fs		! save fs
    o16	push	gs		! save gs
	mov	dx, ss		! ss is kernel data segment
	mov	ds, dx		! load rest of kernel segments
	mov	es, dx		! kernel does not use fs, gs
	mov	eax, esp	! prepare to return
	incb	(_k_reenter)	! from -1 if not reentering  
	!n Save uses the variable _k_reenter to count and determine the level of nesting of interrupts. If a process was executing when the current interrupt occurred  ,'mov esp, k_stktop' instruction switches to the kernel stack
	
	!n If an interrupt could occur while the kernel stack were already in use the address of restart1 would be pushed instead. Of course, an interrupt is not allowed here, but the mechanism is here to handle exceptions. In either case, with a possibly different stack in use from the one that was in effect upon entry, and with the return address in the routine that called it buried beneath the registers that have just been pushed, an ordinary return instruction is not adequate for returning to the caller. 

/*Reentrancy in the kernel causes many problems, and eliminating it resulted in simplification of code in several places. In MINIX 3 the _k_reenter variable still has a purposealthough ordinary interrupts cannot occur while kernel code is executing exceptions are still possible. For now, the thing to keep in mind is that the jump on the following line   will never occur in normal operation. It is, however, necessary for dealing with exceptions.

*/
	jnz	set_restart1	! stack is already kernel stack
	mov	esp, k_stktop
	push	_restart	! build return address for int handler
	xor	ebp, ebp	! for stacktrace
	jmp	RETADR-P_STACKBASE(eax)  !n  use the address that was pushed when save was called. def in sconst.h


	.align	4
set_restart1:
	push	restart1
	jmp	RETADR-P_STACKBASE(eax)    !n  use the address that was pushed when save was called.


!*===========================================================================*
!*				_s_call					     *
!*===========================================================================*
	.align	16
	!n used in gate_table[]  in protect.c prot_init(),which is called by cstart during init 
	!n  Control arrives at _s_call following a software interrupt, that is, execution of an int <nnn> instruction. 
	!n falls to _restart instead of ret/jmp
	!n when _s_call is entered, the CPU has already switched to a stack inside the process table (supplied by the Task State Segment), and several registers have already been pushed onto this stack. 

	/*n
	_p_s_call  is a vestige of the 16-bit version of MINIX 3, which has separate routines for protected mode and real mode operation. In the 32-bit version all calls to either label end up here. 

	A programmer invoking a MINIX 3 system call writes a function call in C that looks like any other function call, whether to a locally defined function or to a routine in the C library. The library code supporting a system call sets up a message, loads the address of the message and the process id of the destination into CPU registers, and then invokes an int SYS386_VECTOR instruction.   the result is that control passes to the start of _s_call, and several registers have already been pushed onto a stack inside the process table. All interrupts are disabled, too, as with a hardware interrupt.

	*/
	//n added to gate table in prot_init
_s_call:
_p_s_call:
	cld			! set direction flag to a known value
	sub	esp, 6*4	! skip RETADR, eax, ecx, edx, ebx, est
	push	ebp		! stack already points into proc table
	push	esi
	push	edi
    o16	push	ds
    o16	push	es
    o16	push	fs
    o16	push	gs
	mov	dx, ss
	mov	ds, dx
	mov	es, dx
	incb	(_k_reenter)
	mov	esi, esp	! assumes P_STACKBASE == 0
	mov	esp, k_stktop
	xor	ebp, ebp	! for stacktrace
				! end of inline save
				! now set up parameters for sys_call()
	push	ebx		! pointer to user message	!n eax ebx ecs are set at __sendrec
	push	eax		! src/dest
	push	ecx		! SEND/RECEIVE/BOTH
	call	_sys_call	! sys_call(function, src_dest, m_ptr)
				! caller is now explicitly in proc_ptr
	mov	AXREG(esi), eax	! sys_call MUST PRESERVE si

! Fall into code to restart proc/task running.

!*===========================================================================*
!*				restart					     *
!*===========================================================================*
/*n
The first time this is called is at the end of main() in kernel/main.c

_restart causes a context switch, so the process pointed to by proc_ptr will run. 

When _restart has executed for the first time we can say that MINIX 3 is running --it is executing a process. 

_Restart is executed again and again as tasks, servers, and user processes are given their opportunities to run and then are suspended, either to wait for input or to give other processes their turns.

ref p166

*/
/*
_restart (line 6681) is reached in several ways:

By a call from main when the system starts.

By a jump from hwint_master or hwint_slave after a hardware interrupt.

By falling through from _s_call after a system call.

Fig. 2-41 is a simplified summary of how control passes back and forth between processes and the kernel via_restart.

ref book 176
*/
_restart:

! Restart the current process or the next process if it is set. 

	cmp	(_next_ptr), 0		! see if another process is scheduled
	jz	0f
	mov 	eax, (_next_ptr)
	mov	(_proc_ptr), eax	! schedule new process 
	mov	(_next_ptr), 0
/*n
 By line 0: the next process to run has been definitively chosen, and with interrupts disabled it cannot be changed. The process table was carefully constructed so it begins with a stack frame, and the instruction on this line,
mov	esp, (_proc_ptr)    
points the CPU's stack pointer register at the stack frame.
*/

0:	mov	esp, (_proc_ptr)	! will assume P_STACKBASE == 0
/*n
The

lldt P_LDT_SEL(esp)

instruction then loads the processor's local descriptor table register from the stack frame. This prepares the processor to use the memory segments belonging to the next process to be run. 

The following instruction sets the address in the next process' process table entry to that where the stack for the next interrupt will be set up, and the following instruction stores this address into the TSS.

*/
	lldt	P_LDT_SEL(esp)		! enable process' segment descriptors 
	lea	eax, P_STACKTOP(esp)	! arrange for next interrupt   //n load effective address to eax
	mov	(_tss+ TSS3_S_SP0), eax	! to save state in process table
/*n
The first part of _restart would not be necessary if an interrupt occured when kernel code (including interrupt service code) were executing, since the kernel stack would be in use and termination of the interrupt service would allow the kernel code to continue. But, in fact, the kernel is not reentrant in MINIX 3, and ordinary interrupts cannot occur this way. However, disabling interrupts does not disable the ability of the processor to detect exceptions. The label restart1 (line 6694) marks the point where execution resumes if an exception occurs while executing kernel code (something we hope will never happen). At this point k_reenter is decremented to record that one level of possibly nested interrupts has been disposed of, and the remaining instructions restore the processor to the state it was in when the next process executed last.
*/
restart1:
	decb	(_k_reenter)
    o16	pop	gs
    o16	pop	fs
    o16	pop	es
    o16	pop	ds
	popad
	add	esp, 4		! skip return adr   !n modifies the stack pointer so the return address that was pushed when save was called is ignored. 
	iretd			! continue process
	!n If the last interrupt occurred when a process was executing, the final instruction, iretd, completes the return to execution of whatever process is being allowed to run next, restoring its remaining registers, including its stack segment and stack pointer. If, however, this encounter with the iretd came via restart1, the kernel stack in use is not a stackframe, but the kernel stack, and this is not a return to an interrupted process, but the completion of handling an exception that occurred while kernel code was executing. The CPU detects this when the code segment descriptor is popped from the stack during execution of the iretd, and the complete action of the iretd in this case is to retain the kernel stack in use.


!*===========================================================================*
!*				exception handlers			     *
!*===========================================================================*
!n An exception is caused by various error conditions internal to the CPU. Exceptions are not always bad. They can be used to stimulate the operating system to provide a service, such as providing more memory for a process to use, or swapping in a currently swapped-out memory page, although such services are not implemented in MINIX 3. 

!n They also can be caused by programming errors. Within the kernel an exception is very serious, and grounds to panic. When an exception occurs in a user program the program may need to be terminated, but the operating system should be able to continue.

!n Exceptions are handled by the same mechanism as interrupts, using descriptors in the interrupt descriptor table. These entries in the table point to the sixteen exception handler entry points, beginning with _divide_error and ending with _copr_error

!n  These all jump to exception (line 6774) or errexception (line 6785) depending upon whether the condition pushes an error code onto the stack or not. C routine _exception  is called to handle the event.The consequences of exceptions vary. Some are ignored, some cause panics, and some result in sending signals to processes. 

_divide_error:
	push	DIVIDE_VECTOR
	jmp	exception

_single_step_exception:
	push	DEBUG_VECTOR
	jmp	exception

_nmi:
	push	NMI_VECTOR
	jmp	exception

_breakpoint_exception:
	push	BREAKPOINT_VECTOR
	jmp	exception

_overflow:
	push	OVERFLOW_VECTOR
	jmp	exception

_bounds_check:
	push	BOUNDS_VECTOR
	jmp	exception

_inval_opcode:
	push	INVAL_OP_VECTOR
	jmp	exception

_copr_not_available:
	push	COPROC_NOT_VECTOR
	jmp	exception

_double_fault:
	push	DOUBLE_FAULT_VECTOR
	jmp	errexception

_copr_seg_overrun:
	push	COPROC_SEG_VECTOR
	jmp	exception

_inval_tss:
	push	INVAL_TSS_VECTOR
	jmp	errexception

_segment_not_present:
	push	SEG_NOT_VECTOR
	jmp	errexception

_stack_exception:
	push	STACK_FAULT_VECTOR
	jmp	errexception

_general_protection:
	push	PROTECTION_VECTOR
	jmp	errexception

_page_fault:
	push	PAGE_FAULT_VECTOR
	jmp	errexception

_copr_error:
	push	COPROC_ERR_VECTOR
	jmp	exception

!*===========================================================================*
!*				exception				     *
!*===========================================================================*
! This is called for all exceptions which do not push an error code.

	.align	16
exception:
 sseg	mov	(trap_errno), 0		! clear trap_errno
 sseg	pop	(ex_number)
	jmp	exception1

!*===========================================================================*
!*				errexception				     *
!*===========================================================================*
! This is called for all exceptions which push an error code.

	.align	16
errexception:
 sseg	pop	(ex_number)
 sseg	pop	(trap_errno)
exception1:				! Common for all exceptions.
	push	eax			! eax is scratch register
	mov	eax, 0+4(esp)		! old eip
 sseg	mov	(old_eip), eax
	movzx	eax, 4+4(esp)		! old cs
 sseg	mov	(old_cs), eax
	mov	eax, 8+4(esp)		! old eflags
 sseg	mov	(old_eflags), eax
	pop	eax
	call	save
	push	(old_eflags)
	push	(old_cs)
	push	(old_eip)
	push	(trap_errno)
	push	(ex_number)
	call	_exception		! (ex_number, trap_errno, old_eip,
					!	old_cs, old_eflags)
	add	esp, 5*4
	ret

!*===========================================================================*
!*				level0_call				     *
!*===========================================================================*
!n This is used when code must be run with privilege level 0, the most privileged level. The entry point is here in mpx386.s with the interrupt and exception entry points because it too is invoked by execution of an int <nnn> instruction.  
!n Like the exception routines, it calls save, and thus the code that is jumped to eventually will terminate with a ret that leads to _restart.
!n Its usage will be described in a later section, when we encounter some code that needs privileges normally not available, even to the kernel.

_level0_call:
	call	save
	jmp	(_level0_func)

!*===========================================================================*
!*				data					     *
!*===========================================================================*
/*n Finally, some data storage space is reserved at the end of the assembly language file. Two different data segments are defined here. The

.sect .rom


declaration at line 6822 ensures that this storage space is allocated at the very beginning of the kernel's data segment and that it is the start of a read-only section of memory. The compiler puts a magic number here so boot can verify that the file it loads is a valid kernel image. When compiling the complete system various string constants will be stored following this. 
*/

.sect .rom	! Before the string table please
	.data2	0x526F		! this must be the first data entry (magic #)

!n .sect .bss  reserves space in the kernel's normal uninitialized variable area for the kernel stack, 
!n and above that at k_stktop  some space is reserved for variables used by the exception handlers. Servers and ordinary processes have stack space reserved when an executable file is linked and depend upon the kernel to properly set the stack segment descriptor and the stack pointer when they are executed. The kernel has to do this for itself.
.sect .bss
k_stack:
	.space	K_STACK_BYTES	! kernel stack
k_stktop:			! top of kernel stack
	.comm	ex_number, 4
	.comm	trap_errno, 4
	.comm	old_eip, 4
	.comm	old_cs, 4
	.comm	old_eflags, 4
/*
ref [1] THE MINIX ASSEMBLY LANGUAGE MANUAL http://minix1.woodhull.com/faq/MinixAsMn.html
*/