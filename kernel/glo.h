#ifndef GLO_H
#define GLO_H

/* Global variables used in the kernel. This file contains the declarations;
 * storage space for the variables is allocated in table.c, because EXTERN is
 * defined as extern unless the _TABLE definition is seen. We rely on the 
 * compiler's default initialization (0) for several global variables. 
 */
#ifdef _TABLE
#undef EXTERN
#define EXTERN
#endif

#include <minix/config.h>
#include "config.h"

/* Variables relating to shutting down MINIX. */
EXTERN char kernel_exception;		/* TRUE after system exceptions */
EXTERN char shutdown_started;		/* TRUE after shutdowns / reboots */

/* Kernel information structures. This groups vital kernel information. */
EXTERN phys_bytes aout;			/* address of a.out headers */
EXTERN struct kinfo kinfo;		/* kernel information for users */			//n  ub cstart
EXTERN struct machine machine;		/* machine information for users */	//n  ub cstart
EXTERN struct kmessages kmess;  	/* diagnostic messages in kernel */
EXTERN struct randomness krandom;	/* gather kernel random information */

/* Process scheduling information and the kernel reentry count. */
EXTERN struct proc *prev_ptr;	/* previously running process */  //n used in clocl.c do_clocktick clock_handler(),defined in sched() in proc.c
EXTERN struct proc *proc_ptr;	/* pointer to currently running process */  //n used in clock_handler(hook) sys_call()
//n modified in _restart(mpx386.s):mov	(_proc_ptr), eax; 
EXTERN struct proc *next_ptr;	/* next process to run after restart() */   //n used and modified in _restart(mpx386.s), and in pick_proc() in proc.c
EXTERN struct proc *bill_ptr;	/* process to bill for clock ticks */   //n used in: clock_handler() in clock.c
EXTERN char k_reenter;		/* kernel reentry count (entry count less 1) */

/*n  This variable is provided for the use of any function that executes in kernel space that might disable interrupts long enough that one or more clock ticks could be lost. 
It currently is used by the int86 function in klib386.s. Int86 uses the boot monitor to manage the transfer of control to the BIOS, and the monitor returns the number of clock ticks counted while the BIOS call was busy in the ecx register just before the return to the kernel. This works because, although the clock chip is not triggering the MINIX 3 clock interrupt handler when the BIOS request is handled, the boot monitor can keep track of the time with the help of the BIOS.
*/
EXTERN unsigned lost_ticks;	/* clock ticks counted outside clock task */  //n set at clock_handler() & csinit in klib386.s

#if (CHIP == INTEL)

/* Interrupt related variables. */
//n for interrupt handling,see "interrupt handlers' in mpx386.s
EXTERN irq_hook_t irq_hooks[NR_IRQ_HOOKS];	/* hooks for general use */
EXTERN irq_hook_t *irq_handlers[NR_IRQ_VECTORS];/* list of IRQ handlers */   //n ub put_irq_handler  ,  hwint_master in mpx386.s,rm_irq_handler
EXTERN int irq_actids[NR_IRQ_VECTORS];		/* IRQ ID bits active */	//n set at intr_handle, ub  hwint_master
EXTERN int irq_use;				/* map of all in-use irq's */		//n put_irq_handler  rm_irq_handler

/* Miscellaneous. */
EXTERN reg_t mon_ss, mon_sp;		/* boot monitor stack */
EXTERN int mon_return;			/* true if we can return to monitor */

/* Variables that are initialized elsewhere are just extern here. */
extern struct boot_image image[]; 	/* system image processes */
extern char *t_stack[];			/* task stack space */
extern struct segdesc_s gdt[];		/* global descriptor table */

EXTERN _PROTOTYPE( void (*level0_func), (void) );
#endif /* (CHIP == INTEL) */

#if (CHIP == M68000)
/* M68000 specific variables go here. */
#endif

#endif /* GLO_H */
