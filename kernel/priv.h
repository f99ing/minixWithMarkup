#ifndef PRIV_H
#define PRIV_H

/* Declaration of the system privileges structure. It defines flags, system 
 * call masks, an synchronous alarm timer, I/O privileges, pending hardware 
 * interrupts and notifications, and so on.
 * System processes each get their own structure with properties, whereas all 
 * user processes share one structure. This setup provides a clear separation
 * between common and privileged process fields and is very space efficient. 
 *
 * Changes:
 *   Jul 01, 2005	Created.  (Jorrit N. Herder)	
 */
#include <minix/com.h>
#include "protect.h"
#include "const.h"
#include "type.h"
 
struct priv {
  proc_nr_t s_proc_nr;		/* number of associated process */
  sys_id_t s_id;		/* index of this system structure */
  short s_flags;		/* PREEMTIBLE, BILLABLE, etc. */  //n used in do_clocktick in clock.c, sys_call()

  short s_trap_mask;		/* allowed system call traps */	//n sys_call
  sys_map_t s_ipc_from;		/* allowed callers to receive from */
  sys_map_t s_ipc_to;		/* allowed destination processes */  //n sys_call
  long s_call_mask;		/* allowed kernel calls */

  sys_map_t s_notify_pending;  	/* bit map with pending notifications */  //n mini_receive mini_notify
  irq_id_t s_int_pending;	/* pending hardware interrupts */	//n ub BuildMess generic_handler
  sigset_t s_sig_pending;	/* pending signals */		//n ub BuildMess

  timer_t s_alarm_timer;	/* synchronous alarm timer */ 
  struct far_mem s_farmem[NR_REMOTE_SEGS];  /* remote memory map */
  reg_t *s_stack_guard;		/* stack guard word for kernel tasks */ //n see dequeue()
};

/* Guard word for task stacks. */
#define STACK_GUARD	((reg_t) (sizeof(reg_t) == 2 ? 0xBEEF : 0xDEADBEEF))

/* Bits for the system property flags. */
#define PREEMPTIBLE	0x01	/* kernel tasks are not preemptible */  //n used in priv.s_flags, do_clocktick in clock.c , IDL_F
#define BILLABLE	0x04	/* some processes are not billable */	//n ub  pick_proc(), IDL_F
#define SYS_PROC	0x10	/* system processes are privileged */   //n used in get_priv(), IDL_F
#define SENDREC_BUSY	0x20	/* sendrec() in progress */   //n ub mini_receive, set at sys_call

/* Magic system structure table addresses. */
#define BEG_PRIV_ADDR (&priv[0])
#define END_PRIV_ADDR (&priv[NR_SYS_PROCS])

#define priv_addr(i)      (ppriv_addr)[(i)]
#define priv_id(rp)	  ((rp)->p_priv->s_id)
#define priv(rp)	  ((rp)->p_priv)  //n used in BuildMess of proc.c

#define id_to_nr(id)	priv_addr(id)->s_proc_nr
#define nr_to_id(nr)    priv(proc_addr(nr))->s_id

/* The system structures table and pointers to individual table slots. The 
 * pointers allow faster access because now a process entry can be found by 
 * indexing the psys_addr array, while accessing an element i requires a 
 * multiplication with sizeof(struct sys) to determine the address. 
 */
EXTERN struct priv priv[NR_SYS_PROCS];		/* system properties table */
EXTERN struct priv *ppriv_addr[NR_SYS_PROCS];	/* direct slot pointers */  //n inited in main()

/* Unprivileged user processes all share the same privilege structure.
 * This id must be fixed because it is used to check send mask entries.
 */
#define USER_PRIV_ID	0

/* Make sure the system can boot. The following sanity check verifies that
 * the system privileges table is large enough for the number of processes
 * in the boot image. 
 */
#if (NR_BOOT_PROCS > NR_SYS_PROCS)
#error NR_SYS_PROCS must be larger than NR_BOOT_PROCS
#endif

#endif /* PRIV_H */
