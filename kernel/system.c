/* This task provides an interface between the kernel and user-space system
 * processes. System services can be accessed by doing a kernel call. Kernel
 * calls are  transformed into request messages, which are handled by this
 * task. By convention, a sys_call() is transformed in a SYS_CALL request
 * message that is handled in a function named do_call(). 
 *
 * A private call vector is used to map all kernel calls to the functions that
 * handle them. The actual handler functions are contained in separate files
 * to keep this file clean. The call vector is used in the system task's main
 * loop to handle all incoming requests.  
 *
 * In addition to the main sys_task() entry point, which starts the main loop,
 * there are several other minor entry points:
 *   get_priv:		assign privilege structure to user or system process
 *   send_sig:		send a signal directly to a system process
 *   cause_sig:		take action to cause a signal to occur via PM
 *   umap_local:	map virtual address in LOCAL_SEG to physical 
 *   umap_remote:	map virtual address in REMOTE_SEG to physical 
 *   umap_bios:		map virtual address in BIOS_SEG to physical 
 *   virtual_copy:	copy bytes from one virtual address to another 
 *   get_randomness:	accumulate randomness in a buffer
 *
 * Changes:
 *   Aug 04, 2005   check if kernel call is allowed  (Jorrit N. Herder)
 *   Jul 20, 2005   send signal to services with message  (Jorrit N. Herder) 
 *   Jan 15, 2005   new, generalized virtual copy function  (Jorrit N. Herder)
 *   Oct 10, 2004   dispatch system calls from call vector  (Jorrit N. Herder)
 *   Sep 30, 2004   source code documentation updated  (Jorrit N. Herder)
 */

#include "kernel.h"
#include "system.h"
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <sys/sigcontext.h>
#if (CHIP == INTEL)
#include <ibm/memory.h>
#include "protect.h"
#endif
/*n
 After doing necessary initialization it runs in a loop. It gets a message, dispatches to the appropriate service procedure, and then sends a reply. A few general support functions are found in the main file, system.c, but the main loop dispatches to a procedure in a separate file in the kernel/system/ directory to process each kernel call

*/

/*
The system task accepts 28 kinds of messages, shown in Fig. 2-45. Each of these can be considered a kernel call, although, as we shall see, in some cases there are multiple macros defined with different names that all result in just one of the message types shown in the figure. And in some other cases more than one of the message types in the figure are handled by a single procedure that does the work.

The message types in Fig. 2-45 fall into several categories. 

The first few are involved with process management. 
.Sys_fork, sys_exec, sys_exit, and sys_trace are obviously closely related to standard POSIX system calls. 
.Although nice is not a POSIX-required system call, the command ultimately results in a sys_nice kernel call to change the priority of a process. 
.The only one of this group that is likely to be unfamiliar is sys_privctl. It is used by the reincarnation server (RS), the MINIX 3 component responsible for converting processes started as ordinary user processes into system processes. Sys_privctl changes the privileges of a process, for instance, to allow it to make kernel calls. Sys_privctl is used when drivers and servers that are not part of the boot image are started by the /etc/rc script. MINIX 3 drivers also can be started (or restarted) at any time; privilege changes are needed whenever this is done.

The next group of kernel calls are related to signals. 
.Sys_kill is related to the user-accessible (and misnamed) system call kill. 
.The others in this group, sys_getksig, sys_endksig, sys_sigsend, and sys_sigreturn are all used by the process manager to get the kernel's help in handling signals.

The sys_irqctl, sys_devio, sys_sdevio, and sys_vdevio kernel calls are unique to MINIX 3. These provide the support needed for user-space device drivers. 
.sys_irqctl One of its functions is to set a hardware interrupt handler and enable interrupts on behalf of a user-space driver.
.Sys_devio allows a user-space driver to ask the system task to read or write from an I/O port.
. The next two kernel calls offer a higher level of I/O device support. 
Sys_sdevio can be used when a sequence of bytes or words, i.e., a string, is to be read from or written to a single I/O address, as might be the case when accessing a serial port. 
Sys_vdevio is used to send a vector of I/O requests to the system task. By a vector is meant a series of (port, value) pairs. done by a user-space process a single message passing the address to a buffer containing 10 (port, value) pairs would be much more efficient than 10 messages each passing one port address and a value to be written.

The next three kernel calls shown in Fig. 2-45 involve memory in distinct ways. 
.The first, sys_newmap, is called by the process manager when the memory used by a process changes, so the kernel's part of the process table can be updated. .Sys_segctl and sys_memset provide a safe way to provide a process with access to memory outside its own data space. 
The memory area from 0xa0000 to 0xfffff is reserved for I/O devices, as we mentioned in the discussion of startup of the MINIX 3 system. Some devices use part of this memory region for I/Ofor instance, video display cards expect to have data to be displayed written into memory on the card which is mapped here. Sys_segctl is used by a device driver to obtain a segment selector that will allow it to address memory in this range. 
The other call, sys_memset, is used when a server wants to write data into an area of memory that does not belong to it. It is used by the process manager to zero out memory when a new process is started, to prevent the new process from reading data left by another process.

The next group of kernel calls is for copying memory. 
.Sys_umap converts virtual addresses to physical addresses. 
.Sys_vircopy and sys_physcopy copy regions of memory, using either virtual or physical addresses. 
.The next two calls, sys_virvcopy and sys_physvcopy are vector versions of the previous two. As with vectored I/O requests, these allow making a request to the system task for a series of memory copy operations.

some are left after p197 before 2.7.2



*/
/*n
The system task is compiled from a header, system.h, and a C source file, system.c, in the main kernel/ directory. In addition there is a specialized library built from source files in a subdirectory, kernel/system/. 
There is a reason for this organization. Although MINIX 3 as we describe it here is a general-purpose operating system, it is also potentially useful for special purposes, such as embedded support in a portable device. In such cases a stripped-down version of the operating system might be adequate. For instance, a device without a disk might not need a file system. We saw in kernel/config.h that compilation of kernel calls can be selectively enabled and disabled. Having the code that supports each kernel call linked from the library as the last stage of compilation makes it easier to build a customized system.


*/
/* Declaration of the call vector that defines the mapping of kernel calls 
 * to handler functions. The vector is initialized in sys_init() with map(), 
 * which makes sure the kernel call numbers are ok. No space is allocated, 
 * because the dummy is declared extern. If an illegal call is given, the 
 * array size will be negative and this won't compile. 
 */
PUBLIC int (*call_vec[NR_SYS_CALLS])(message *m_ptr);

#define map(call_nr, handler) \
    {extern int dummy[NR_SYS_CALLS>(unsigned)(call_nr-KERNEL_CALL) ? 1:-1];} \
    call_vec[(call_nr-KERNEL_CALL)] = (handler)  

FORWARD _PROTOTYPE( void initialize, (void));

/*===========================================================================*
 *				sys_task				     *
 *===========================================================================*/
PUBLIC void sys_task()
{
/* Main entry point of sys_task.  Get the message and dispatch on type. */
  static message m;
  register int result;
  register struct proc *caller_ptr;
  unsigned int call_nr;
  int s;

  /* Initialize the system task. */
  initialize();

  while (TRUE) {
      /* Get work. Block and wait until a request message arrives. */
      receive(ANY, &m);			
      call_nr = (unsigned) m.m_type - KERNEL_CALL;	
      caller_ptr = proc_addr(m.m_source);	

      /* See if the caller made a valid request and try to handle it. */
      if (! (priv(caller_ptr)->s_call_mask & (1<<call_nr))) {
	  kprintf("SYSTEM: request %d from %d denied.\n", call_nr,m.m_source);
	  result = ECALLDENIED;			/* illegal message type */
      } else if (call_nr >= NR_SYS_CALLS) {		/* check call number */
	  kprintf("SYSTEM: illegal request %d from %d.\n", call_nr,m.m_source);
	  result = EBADREQUEST;			/* illegal message type */
      } 
      else {
          result = (*call_vec[call_nr])(&m);	/* handle the kernel call */
      }

      /* Send a reply, unless inhibited by a handler function. Use the kernel
       * function lock_send() to prevent a system call trap. The destination
       * is known to be blocked waiting for a message.
       */
      if (result != EDONTREPLY) {
  	  m.m_type = result;			/* report status of call */
          if (OK != (s=lock_send(m.m_source, &m))) {
              kprintf("SYSTEM, reply to %d failed: %d\n", m.m_source, s);
          }
      }
  }
}

/*===========================================================================*
 *				initialize				     *
 *===========================================================================*/
PRIVATE void initialize(void)
{
  register struct priv *sp;
  int i;

  /* Initialize IRQ handler hooks. Mark all hooks available. */
  for (i=0; i<NR_IRQ_HOOKS; i++) {
      irq_hooks[i].proc_nr = NONE;
  }

  /* Initialize all alarm timers for all processes. */
  for (sp=BEG_PRIV_ADDR; sp < END_PRIV_ADDR; sp++) {
    tmr_inittimer(&(sp->s_alarm_timer));
  }

  /* Initialize the call vector to a safe default handler. Some kernel calls 
   * may be disabled or nonexistant. Then explicitly map known calls to their
   * handler functions. This is done with a macro that gives a compile error
   * if an illegal call number is used. The ordering is not important here.
   */
  for (i=0; i<NR_SYS_CALLS; i++) {
      call_vec[i] = do_unused;
  }

  /* Process management. */
  map(SYS_FORK, do_fork); 		/* a process forked a new process */
  map(SYS_EXEC, do_exec);		/* update process after execute */
  map(SYS_EXIT, do_exit);		/* clean up after process exit */
  map(SYS_NICE, do_nice);		/* set scheduling priority */
  map(SYS_PRIVCTL, do_privctl);		/* system privileges control */
  map(SYS_TRACE, do_trace);		/* request a trace operation */

  /* Signal handling. */
  map(SYS_KILL, do_kill); 		/* cause a process to be signaled */
  map(SYS_GETKSIG, do_getksig);		/* PM checks for pending signals */
  map(SYS_ENDKSIG, do_endksig);		/* PM finished processing signal */
  map(SYS_SIGSEND, do_sigsend);		/* start POSIX-style signal */
  map(SYS_SIGRETURN, do_sigreturn);	/* return from POSIX-style signal */

  /* Device I/O. */
  map(SYS_IRQCTL, do_irqctl);  		/* interrupt control operations */ 
  map(SYS_DEVIO, do_devio);   		/* inb, inw, inl, outb, outw, outl */ 
  map(SYS_SDEVIO, do_sdevio);		/* phys_insb, _insw, _outsb, _outsw */
  map(SYS_VDEVIO, do_vdevio);  		/* vector with devio requests */ 
  map(SYS_INT86, do_int86);  		/* real-mode BIOS calls */ 

  /* Memory management. */
  map(SYS_NEWMAP, do_newmap);		/* set up a process memory map */
  map(SYS_SEGCTL, do_segctl);		/* add segment and get selector */
  map(SYS_MEMSET, do_memset);		/* write char to memory area */

  /* Copying. */
  map(SYS_UMAP, do_umap);		/* map virtual to physical address */
  map(SYS_VIRCOPY, do_vircopy); 	/* use pure virtual addressing */
  map(SYS_PHYSCOPY, do_physcopy); 	/* use physical addressing */
  map(SYS_VIRVCOPY, do_virvcopy);	/* vector with copy requests */
  map(SYS_PHYSVCOPY, do_physvcopy);	/* vector with copy requests */

  /* Clock functionality. */
  map(SYS_TIMES, do_times);		/* get uptime and process times */
  map(SYS_SETALARM, do_setalarm);	/* schedule a synchronous alarm */

  /* System control. */
  map(SYS_ABORT, do_abort);		/* abort MINIX */
  map(SYS_GETINFO, do_getinfo); 	/* request system information */ 
}

/*===========================================================================*
 *				get_priv				     *
 *===========================================================================*/
PUBLIC int get_priv(rc, proc_type)
register struct proc *rc;		/* new (child) process pointer */
int proc_type;				/* system or user process flag */
{
/* Get a privilege structure. All user processes share the same privilege 
 * structure. System processes get their own privilege structure. 
 */
  register struct priv *sp;			/* privilege structure */

  if (proc_type == SYS_PROC) {			/* find a new slot */
      for (sp = BEG_PRIV_ADDR; sp < END_PRIV_ADDR; ++sp) 
          if (sp->s_proc_nr == NONE && sp->s_id != USER_PRIV_ID) break;	
      if (sp->s_proc_nr != NONE) return(ENOSPC);
      rc->p_priv = sp;				/* assign new slot */
      rc->p_priv->s_proc_nr = proc_nr(rc);	/* set association */
      rc->p_priv->s_flags = SYS_PROC;		/* mark as privileged */
  } else {
      rc->p_priv = &priv[USER_PRIV_ID];		/* use shared slot */
      rc->p_priv->s_proc_nr = INIT_PROC_NR;	/* set association */
      rc->p_priv->s_flags = 0;			/* no initial flags */
  }
  return(OK);
}

/*===========================================================================*
 *				get_randomness				     *
 *===========================================================================*/
PUBLIC void get_randomness(source)
int source;
{
/* On machines with the RDTSC (cycle counter read instruction - pentium
 * and up), use that for high-resolution raw entropy gathering. Otherwise,
 * use the realtime clock (tick resolution).
 *
 * Unfortunately this test is run-time - we don't want to bother with
 * compiling different kernels for different machines.
 *
 * On machines without RDTSC, we use read_clock().
 */
  int r_next;
  unsigned long tsc_high, tsc_low;

  source %= RANDOM_SOURCES;
  r_next= krandom.bin[source].r_next;
  if (machine.processor > 486) {
      read_tsc(&tsc_high, &tsc_low);
      krandom.bin[source].r_buf[r_next] = tsc_low;
  } else {
      krandom.bin[source].r_buf[r_next] = read_clock();
  }
  if (krandom.bin[source].r_size < RANDOM_ELEMENTS) {
  	krandom.bin[source].r_size ++;
  }
  krandom.bin[source].r_next = (r_next + 1 ) % RANDOM_ELEMENTS;
}

/*===========================================================================*
 *				send_sig				     *
 *===========================================================================*/
PUBLIC void send_sig(proc_nr, sig_nr)
int proc_nr;			/* system process to be signalled */
int sig_nr;			/* signal to be sent, 1 to _NSIG */
{
/* Notify a system process about a signal. This is straightforward. Simply
 * set the signal that is to be delivered in the pending signals map and 
 * send a notification with source SYSTEM.
 */ 
  register struct proc *rp;

  rp = proc_addr(proc_nr);
  sigaddset(&priv(rp)->s_sig_pending, sig_nr);
  lock_notify(SYSTEM, proc_nr); 
}

/*===========================================================================*
 *				cause_sig				     *
 *===========================================================================*/
PUBLIC void cause_sig(proc_nr, sig_nr)
int proc_nr;			/* process to be signalled */
int sig_nr;			/* signal to be sent, 1 to _NSIG */
{
/* A system process wants to send a signal to a process.  Examples are:
 *  - HARDWARE wanting to cause a SIGSEGV after a CPU exception
 *  - TTY wanting to cause SIGINT upon getting a DEL
 *  - FS wanting to cause SIGPIPE for a broken pipe 
 * Signals are handled by sending a message to PM.  This function handles the 
 * signals and makes sure the PM gets them by sending a notification. The 
 * process being signaled is blocked while PM has not finished all signals 
 * for it. 
 * Race conditions between calls to this function and the system calls that
 * process pending kernel signals cannot exist. Signal related functions are
 * only called when a user process causes a CPU exception and from the kernel 
 * process level, which runs to completion.
 */
  register struct proc *rp;

  /* Check if the signal is already pending. Process it otherwise. */
  rp = proc_addr(proc_nr);
  if (! sigismember(&rp->p_pending, sig_nr)) {
      sigaddset(&rp->p_pending, sig_nr);
      if (! (rp->p_rts_flags & SIGNALED)) {		/* other pending */
          if (rp->p_rts_flags == 0) lock_dequeue(rp);	/* make not ready */
          rp->p_rts_flags |= SIGNALED | SIG_PENDING;	/* update flags */
          send_sig(PM_PROC_NR, SIGKSIG);
      }
  }
}

/*===========================================================================*
 *				umap_local				     *
 *===========================================================================*/
PUBLIC phys_bytes umap_local(rp, seg, vir_addr, bytes)
register struct proc *rp;	/* pointer to proc table entry for process */
int seg;			/* T, D, or S segment */
vir_bytes vir_addr;		/* virtual address in bytes within the seg */
vir_bytes bytes;		/* # of bytes to be copied */
{
/* Calculate the physical memory address for a given virtual address. */
  vir_clicks vc;		/* the virtual address in clicks */
  phys_bytes pa;		/* intermediate variables as phys_bytes */
#if (CHIP == INTEL)
  phys_bytes seg_base;
#endif

  /* If 'seg' is D it could really be S and vice versa.  T really means T.
   * If the virtual address falls in the gap,  it causes a problem. On the
   * 8088 it is probably a legal stack reference, since "stackfaults" are
   * not detected by the hardware.  On 8088s, the gap is called S and
   * accepted, but on other machines it is called D and rejected.
   * The Atari ST behaves like the 8088 in this respect.
   */

  if (bytes <= 0) return( (phys_bytes) 0);
  if (vir_addr + bytes <= vir_addr) return 0;	/* overflow */
  vc = (vir_addr + bytes - 1) >> CLICK_SHIFT;	/* last click of data */

#if (CHIP == INTEL) || (CHIP == M68000)
  if (seg != T)
	seg = (vc < rp->p_memmap[D].mem_vir + rp->p_memmap[D].mem_len ? D : S);
#else
  if (seg != T)
       seg = (vc < rp->p_memmap[S].mem_vir ? D : S);
#endif

  if ((vir_addr>>CLICK_SHIFT) >= rp->p_memmap[seg].mem_vir + 
  	rp->p_memmap[seg].mem_len) return( (phys_bytes) 0 );

  if (vc >= rp->p_memmap[seg].mem_vir + 
  	rp->p_memmap[seg].mem_len) return( (phys_bytes) 0 );

#if (CHIP == INTEL)
  seg_base = (phys_bytes) rp->p_memmap[seg].mem_phys;
  seg_base = seg_base << CLICK_SHIFT;	/* segment origin in bytes */
#endif
  pa = (phys_bytes) vir_addr;
#if (CHIP != M68000)
  pa -= rp->p_memmap[seg].mem_vir << CLICK_SHIFT;
  return(seg_base + pa);
#endif
#if (CHIP == M68000)
  pa -= (phys_bytes)rp->p_memmap[seg].mem_vir << CLICK_SHIFT;
  pa += (phys_bytes)rp->p_memmap[seg].mem_phys << CLICK_SHIFT;
  return(pa);
#endif
}

/*===========================================================================*
 *				umap_remote				     *
 *===========================================================================*/
PUBLIC phys_bytes umap_remote(rp, seg, vir_addr, bytes)
register struct proc *rp;	/* pointer to proc table entry for process */
int seg;			/* index of remote segment */
vir_bytes vir_addr;		/* virtual address in bytes within the seg */
vir_bytes bytes;		/* # of bytes to be copied */
{
/* Calculate the physical memory address for a given virtual address. */
  struct far_mem *fm;

  if (bytes <= 0) return( (phys_bytes) 0);
  if (seg < 0 || seg >= NR_REMOTE_SEGS) return( (phys_bytes) 0);

  fm = &rp->p_priv->s_farmem[seg];
  if (! fm->in_use) return( (phys_bytes) 0);
  if (vir_addr + bytes > fm->mem_len) return( (phys_bytes) 0);

  return(fm->mem_phys + (phys_bytes) vir_addr); 
}

/*===========================================================================*
 *				umap_bios				     *
 *===========================================================================*/
PUBLIC phys_bytes umap_bios(rp, vir_addr, bytes)
register struct proc *rp;	/* pointer to proc table entry for process */
vir_bytes vir_addr;		/* virtual address in BIOS segment */
vir_bytes bytes;		/* # of bytes to be copied */
{
/* Calculate the physical memory address at the BIOS. Note: currently, BIOS
 * address zero (the first BIOS interrupt vector) is not considered as an 
 * error here, but since the physical address will be zero as well, the 
 * calling function will think an error occurred. This is not a problem,
 * since no one uses the first BIOS interrupt vector.  
 */

  /* Check all acceptable ranges. */
  if (vir_addr >= BIOS_MEM_BEGIN && vir_addr + bytes <= BIOS_MEM_END)
  	return (phys_bytes) vir_addr;
  else if (vir_addr >= BASE_MEM_TOP && vir_addr + bytes <= UPPER_MEM_END)
  	return (phys_bytes) vir_addr;
  kprintf("Warning, error in umap_bios, virtual address 0x%x\n", vir_addr);
  return 0;
}

/*===========================================================================*
 *				virtual_copy				     *
 *===========================================================================*/
PUBLIC int virtual_copy(src_addr, dst_addr, bytes)
struct vir_addr *src_addr;	/* source virtual address */
struct vir_addr *dst_addr;	/* destination virtual address */
vir_bytes bytes;		/* # of bytes to copy  */
{
/* Copy bytes from virtual address src_addr to virtual address dst_addr. 
 * Virtual addresses can be in ABS, LOCAL_SEG, REMOTE_SEG, or BIOS_SEG.
 */
  struct vir_addr *vir_addr[2];	/* virtual source and destination address */
  phys_bytes phys_addr[2];	/* absolute source and destination */ 
  int seg_index;
  int i;

  /* Check copy count. */
  if (bytes <= 0) return(EDOM);

  /* Do some more checks and map virtual addresses to physical addresses. */
  vir_addr[_SRC_] = src_addr;
  vir_addr[_DST_] = dst_addr;
  for (i=_SRC_; i<=_DST_; i++) {

      /* Get physical address. */
      switch((vir_addr[i]->segment & SEGMENT_TYPE)) {
      case LOCAL_SEG:
          seg_index = vir_addr[i]->segment & SEGMENT_INDEX;
          phys_addr[i] = umap_local( proc_addr(vir_addr[i]->proc_nr), 
              seg_index, vir_addr[i]->offset, bytes );
          break;
      case REMOTE_SEG:
          seg_index = vir_addr[i]->segment & SEGMENT_INDEX;
          phys_addr[i] = umap_remote( proc_addr(vir_addr[i]->proc_nr), 
              seg_index, vir_addr[i]->offset, bytes );
          break;
      case BIOS_SEG:
          phys_addr[i] = umap_bios( proc_addr(vir_addr[i]->proc_nr),
              vir_addr[i]->offset, bytes );
          break;
      case PHYS_SEG:
          phys_addr[i] = vir_addr[i]->offset;
          break;
      default:
          return(EINVAL);
      }

      /* Check if mapping succeeded. */
      if (phys_addr[i] <= 0 && vir_addr[i]->segment != PHYS_SEG) 
          return(EFAULT);
  }

  /* Now copy bytes between physical addresseses. */
  phys_copy(phys_addr[_SRC_], phys_addr[_DST_], (phys_bytes) bytes);
  return(OK);
}

