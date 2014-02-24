/* This file contains the clock task, which handles time related functions.
 * Important events that are handled by the CLOCK include setting and 
 * monitoring alarm timers and deciding when to (re)schedule processes. 
 * The CLOCK offers a direct interface to kernel processes. System services 
 * can access its services through system calls, such as sys_setalarm(). The
 * CLOCK task thus is hidden from the outside world.  
 *
 * Changes:
 *   Oct 08, 2005   reordering and comment editing (A. S. Woodhull)
 *   Mar 18, 2004   clock interface moved to SYSTEM task (Jorrit N. Herder) 
 *   Sep 30, 2004   source code documentation updated  (Jorrit N. Herder)
 *   Sep 24, 2004   redesigned alarm timers  (Jorrit N. Herder)
 *
 * The function do_clocktick() is triggered by the clock's interrupt 
 * handler when a watchdog timer has expired or a process must be scheduled. 
 *
 * In addition to the main clock_task() entry point, which starts the main 
 * loop, there are several other minor entry points:
 *   clock_stop:	called just before MINIX shutdown
 *   get_uptime:	get realtime since boot in clock ticks
 *   set_timer:		set a watchdog timer (+)
 *   reset_timer:	reset a watchdog timer (+)
 *   read_clock:	read the counter of channel 0 of the 8253A timer
 *
 * (+) The CLOCK task keeps tracks of watchdog timers for the entire kernel.
 * The watchdog functions of expired timers are executed in do_clocktick(). 
 * It is crucial that watchdog functions not block, or the CLOCK task may
 * be blocked. Do not send() a message when the receiver is not expecting it.
 * Instead, notify(), which always returns, should be used. 
 */
 /*n
 clock is built out of three components: a crystal oscillator, a counter, and a holding register
the crystal oscillator generate a periodic signal of very high accuracy, typically in the range of 5 to 200 MHz,This signal is fed into the counter to make it count down to zero. When the counter gets to zero, it causes a CPU interrupt.
.Computers whose advertised clock rate is higher than 200 MHz normally use a slower clock and a clock multiplier circuit.

Programmable clocks typically have several modes of operation. 
.In one-shot mode, when the clock is started, it copies the value of the holding register into the counter and then decrements the counter at each pulse from the crystal. When the counter gets to zero, it causes an interrupt and stops until it is explicitly started again by the software.
.In square-wave mode, after getting to zero and causing the interrupt, the holding register is automatically copied into the counter, and the whole process is repeated again indefinitely. These periodic interrupts are called clock ticks.

process time accounting
 .start a second timer, distinct from the main system timer, whenever a process is started. When that process is stopped, the timer can be read out to tell how long the process has run. To do things right, the second timer should be saved when an interrupt occurs and restored afterward.
 .A less accurate, but much simpler, way to do accounting is to maintain a pointer to the process table entry for the currently running process in a global variable. At every clock tick, a field in the current process' entry is incremented. In this way, every clock tick is "charged" to the process running at the time of the tick.


a process can request that the operating system give it a warning after a certain interval. The warning is usually a signal, interrupt, message, or something similar.
.If the clock driver had enough clocks, it could set a separate clock for each request. This not being the case, it must simulate multiple virtual clocks with a single physical clock. One way is to maintain a table in which the signal time for all pending timers is kept, as well as a variable giving the time of the next one. Whenever the time of day is updated, the driver checks to see if the closest signal has occurred. If it has, the table is searched for the next one to occur.
.If many signals are expected, it is more efficient to simulate multiple clocks by chaining all the pending clock requests together, sorted on time, in a linked list, as shown in Fig. 2-49. Each entry on the list tells how many clock ticks following the previous one to wait before causing a signal.


watchdog timers. 
The mechanism used by the clock driver to handle watchdog timers is the same as for user signals. The only difference is that when a timer goes off, instead of causing a signal, the clock driver calls a procedure supplied by the caller. The procedure is part of the caller's code. This presented a problem in the design of MINIX 3, since one of the goals was to remove drivers from the kernel's address space. The short answer is that the system task, which is in kernel space, can set alarms on behalf of some user-space processes, and then notify them when a timer goes off. We will elaborate on this mechanism further on.

 during a clock interrupt, the clock driver has several things to do. These things include incrementing the real time, decrementing the quantum and checking for 0, doing CPU accounting, and decrementing the alarm counter. However, each of these operations has been carefully arranged to be very fast because they have to be repeated many times a second.

 */
//n The MINIX 3 clock task has some resemblance to a device driver, in that it is driven by interrupts generated by a hardware device. 
/* 1.  Maintaining the time of day.

 
2.  Preventing processes from running longer than they are allowed to.

 
3.  Accounting for CPU usage.

 
4.  Handling the alarm system call made by user processes.

 
5.  Providing watchdog timers for parts of the system itself.

 
6.  Doing profiling, monitoring, and statistics gathering.
 
*/
/*n
 This file can be considered to have three functional parts. 
 
First, like the device drivers , there is a task mechanism which runs in a loop, waiting for messages and dispatching to subroutines that perform the action requested in each message. However, this structure is almost vestigial in the clock task. The message mechanism is expensive, requiring all the overhead of a context switch. So for the clock this is used only when there is a substantial amount of work to be done. Only one kind of message is received, there is only one subroutine to service the message, and a reply message is not sent when the job is done.

The second major part of the clock software is the interrupt handler that is activated 60 times each second. It does basic timekeeping, updating a variable that counts clock ticks since the system was booted. It compares this with the time for the next timer expiration. It also updates counters that register how much of the quantum of the current process has been used and how much total time the current process has used. If the interrupt handler detects that a process has used its quantum or that a timer has expired it generates the message that goes to the main task loop. Otherwise no message is sent. The strategy here is that for each clock tick the handler does as little as necessary, as fast as possible. The costly main task is activated only when there is substantial work to do.

The third general part of the clock software is a collection of subroutines that provide general support, but which are not called in response to clock interrupts, either by the interrupt handler or by the main task loop. One of these subroutines is coded as PRIVATE, and is called before the main task loop is entered. It initializes the clock, which entails writing data to the clock chip to cause it to generate interrupts at the desired intervals. The initialization routine also puts the address of the interrupt handler in the right place to be found when the clock chip triggers the IRQ 8 input to the interrupt controller chip, and then enables that input to respond.
The rest of the subroutines in clock.c are declared PUBLIC, and can be called from anywhere in the kernel binary. In fact none of them are called from clock.c itself. They are mostly called by the system task in order to service system calls related to time. These subroutines do such things as reading the time-since-boot counter, for timing with clock-tick resolution, or reading a register in the clock chip itself, for timing that requires microsecond resolution. Other subroutines are used to set and reset timers. Finally, a subroutine is provided to be called when MINIX 3 shuts down. This one resets the hardware timer parameters to those expected by the BIOS.

clock:¡¡8253A¡¡¡¡
8253A: There are 6 modes in total
Mode 1 (001): Programmable One Shot
Mode 3 (X11): Square Wave Generator

It is organized as 3 independent 16-bit counters, each with a counter rate up to 2 MHz,accessed from port 40h 41h 42h

On PCs the address for timer0 (chip) is at port 40h..43h like described and the second timer1 (chip) is at 50h..53h.

The 82C54 is pin compatible with the HMOS 8254, and is a superset of the 8253.
http://en.wikipedia.org/wiki/Intel_8253  
http://www.intel-assembler.it/portale/5/Programming-the-Intel-8253-8354-pit/howto-program-intel-pit-8253-8254.asp
http://www.sharpmz.org/mz-700/8253ovview.htm
*/
#include "kernel.h"
#include "proc.h"
#include <signal.h>
#include <minix/com.h>

/* Function prototype for PRIVATE functions. */ 
FORWARD _PROTOTYPE( void init_clock, (void) );
FORWARD _PROTOTYPE( int clock_handler, (irq_hook_t *hook) );
FORWARD _PROTOTYPE( int do_clocktick, (message *m_ptr) );

/* Clock parameters. */
#define COUNTER_FREQ (2*TIMER_FREQ) /* counter frequency using square wave */
#define LATCH_COUNT     0x00	/* cc00xxxx, c = channel, x = any */    //n used in read_clock()
//n 0x36=00 11 011 0   channel 0 counter (port 40h),	Read/Load LSB then MSB , mode 3(square wave), Binary 16 bit(1=BCD 4 decades)
#define SQUARE_WAVE     0x36	/* ccaammmb, a = access, m = mode, b = BCD */   //n used in init_clock()
				/*   11x11, 11 = LSB then MSB, x11 = sq wave */
#define TIMER_COUNT ((unsigned) (TIMER_FREQ/HZ)) /* initial value for counter*/   //n used in init_clock()
#define TIMER_FREQ  1193182L	/* clock frequency for timer in PC and AT */

#define CLOCK_ACK_BIT	0x80	/* PS/2 clock interrupt acknowledge bit */   //n ub  clock_handler

/* The CLOCK's timers queue. The functions in <timers.h> operate on this. 
 * Each system process possesses a single synchronous alarm timer. If other 
 * kernel parts want to use additional timers, they must declare their own 
 * persistent (static) timer structure, which can be passed to the clock
 * via (re)set_timer().
 * When a timer expires its watchdog function is run by the CLOCK task. 
 */
PRIVATE timer_t *clock_timers;		/* queue of CLOCK timers */   //n ub do_clocktick set_timer() reset_timer()
PRIVATE clock_t next_timeout;		/* realtime that next timer expires */  //n set in set_timer() reset_timer(), used in handler & do_clocktick

/* The time is incremented by the interrupt handler on each clock tick. */
PRIVATE clock_t realtime;		/* real time clock */		//n set(incremented) at clock_handler() in clock.c , returned by get_uptime
PRIVATE irq_hook_t clock_hook;		/* interrupt handler hook */ //n ub  init_clock()

/*===========================================================================*
 *				clock_task				     *
 *===========================================================================*/
 //n registered in image[] in table.c
 /*n
 The main loop of the clock task accepts only a single kind of message, HARD_INT, which comes from the interrupt handler. Anything else is an error. 
 
 Furthermore, it does not receive this message for every clock tick interrupt, although the subroutine called is named do_clocktick. A message is received, and do_clocktick is called only if process scheduling is needed or a timer has expired.
 */
PUBLIC void clock_task()
{
/* Main program of clock task. If the call is not HARD_INT it is an error.
 */
  message m;			/* message buffer for both input and output */
  int result;			/* result returned by the handler */

  init_clock();			/* initialize clock task */

  /* Main loop of the clock task.  Get work, process it. Never reply. */
  while (TRUE) {

      /* Go get a message. */
      receive(ANY, &m);	

      /* Handle the request. Only clock ticks are expected. */
      switch (m.m_type) {
      case HARD_INT:
          result = do_clocktick(&m);	/* handle clock tick */
          break;
      default:				/* illegal request type */
          kprintf("CLOCK: illegal request %d from %d.\n", m.m_type,m.m_source);
      }
  }
}

/*n
called by clock_task above
The interrupt handler runs every time the counter in the clock chip reaches zero and generates an interrupt. This is where the basic timekeeping work is done. In MINIX 3 the time is kept using the method of Fig. 2-48(c). However, in clock.c only the counter for ticks since boot is maintained; records of the boot time are kept elsewhere. The clock software supplies only the current tick count to aid a system call for the real time. Further processing is done by one of the servers.
*/
/*===========================================================================*
 *				do_clocktick				     *
 *===========================================================================*/
PRIVATE int do_clocktick(m_ptr)
message *m_ptr;				/* pointer to request message */
{
/* Despite its name, this routine is not called on every clock tick. It
 * is called on those clock ticks when a lot of work needs to be done.
 */

  /* A process used up a full quantum. The interrupt handler stored this
   * process in 'prev_ptr'.  First make sure that the process is not on the 
   * scheduling queues.  Then announce the process ready again. Since it has 
   * no more time left, it gets a new quantum and is inserted at the right 
   * place in the queues.  As a side-effect a new process will be scheduled.
   */ 
  if (prev_ptr->p_ticks_left <= 0 && priv(prev_ptr)->s_flags & PREEMPTIBLE) {
      lock_dequeue(prev_ptr);		/* take it off the queues */
      lock_enqueue(prev_ptr);		/* and reinsert it again */ 
  }

  /* Check if a clock timer expired and run its watchdog function. */
  if (next_timeout <= realtime) { 
  	tmrs_exptimers(&clock_timers, realtime, NULL);
  	next_timeout = clock_timers == NULL ? 
		TMR_NEVER : clock_timers->tmr_exp_time;
  }

  /* Inhibit sending a reply. */
  return(EDONTREPLY);
}

/*===========================================================================*
 *				init_clock				     *
 *===========================================================================*/
 //n initialize the programmable clock frequency to 60 Hz.  called by clock_task
PRIVATE void init_clock()
{
  /* Initialize the CLOCK's interrupt hook. */
  clock_hook.proc_nr = CLOCK;

  /* Initialize channel 0 of the 8253A timer to, e.g., 60 Hz. */  //n thus generating 60 interrupt per second?
  outb(TIMER_MODE, SQUARE_WAVE);	/* set timer to run continuously */
  outb(TIMER0, TIMER_COUNT);		/* load timer low byte */
  outb(TIMER0, TIMER_COUNT >> 8);	/* load timer high byte */
  put_irq_handler(&clock_hook, CLOCK_IRQ, clock_handler);/* register handler */
  enable_irq(&clock_hook);		/* ready for clock interrupts */  //n in klib386.s
}

/*===========================================================================*
 *				clock_stop				     *
 *===========================================================================*/
PUBLIC void clock_stop()
{
/* Reset the clock to the BIOS rate. (For rebooting) */
  outb(TIMER_MODE, 0x36);
  outb(TIMER0, 0);
  outb(TIMER0, 0);
}

/*===========================================================================*
 *				clock_handler				     *
 *===========================================================================*/
 //n As soon as (or, more accurately, 16.67 milliseconds after) init_clock runs, the first clock interrupt occurs, and clock interrupts repeat 60 times a second as long as MINIX 3 runs. The code in clock_handler   probably runs more frequently than any other part of the MINIX 3 system. Consequently, clock_handler was built for speed.
 /*n The interrupt handler runs every time the counter in the clock chip reaches zero and generates an interrupt. This is where the basic timekeeping work is done. In MINIX 3 the time is kept using the method of Fig. 2-48(c). However, in clock.c only the counter for ticks since boot is maintained; records of the boot time are kept elsewhere. The clock software supplies only the current tick count to aid a system call for the real time. Further processing is done by one of the servers.
*/
PRIVATE int clock_handler(hook)
irq_hook_t *hook;
{
/* This executes on each clock tick (i.e., every time the timer chip generates 
 * an interrupt). It does a little bit of work so the clock task does not have 
 * to be called on every tick.  The clock task is called when:
 *
 *	(1) the scheduling quantum of the running process has expired, or
 *	(2) a timer has expired and the watchdog function should be run.
 *
 * Many global global and static variables are accessed here.  The safety of
 * this must be justified. All scheduling and message passing code acquires a 
 * lock by temporarily disabling interrupts, so no conflicts with calls from 
 * the task level can occur. Furthermore, interrupts are not reentrant, the 
 * interrupt handler cannot be bothered by other interrupts.
 * 
 * Variables that are updated in the clock's interrupt handler:
 *	lost_ticks:
 *		Clock ticks counted outside the clock task. This for example
 *		is used when the boot monitor processes a real mode interrupt.
 * 	realtime:
 * 		The current uptime is incremented with all outstanding ticks.
 *	proc_ptr, bill_ptr:
 *		These are used for accounting.  It does not matter if proc.c
 *		is changing them, provided they are always valid pointers,
 *		since at worst the previous process would be billed.
 */
  register unsigned ticks;

  /* Acknowledge the PS/2 clock interrupt. */
  //n they are only needed if running on an obsolete IBM PS/2 system.
  if (machine.ps_mca) outb(PORT_B, inb(PORT_B) | CLOCK_ACK_BIT);

  /* Get number of ticks and update realtime. */
  ticks = lost_ticks + 1;
  lost_ticks = 0;	
  realtime += ticks;

  /* Update user and system accounting times. Charge the current process for
   * user time. If the current process is not billable, that is, if a non-user
   * process is running, charge the billable process for system time as well.
   * Thus the unbillable process' user time is the billable user's system time.
   */
  proc_ptr->p_user_time += ticks;
  if (priv(proc_ptr)->s_flags & PREEMPTIBLE) {
      proc_ptr->p_ticks_left -= ticks;
  }
  if (! (priv(proc_ptr)->s_flags & BILLABLE)) {
      bill_ptr->p_sys_time += ticks;
      bill_ptr->p_ticks_left -= ticks;
  }

  /* Check if do_clocktick() must be called. Done for alarms and scheduling.
   * Some processes, such as the kernel tasks, cannot be preempted. 
   */ 
  if ((next_timeout <= realtime) || (proc_ptr->p_ticks_left <= 0)) {
      prev_ptr = proc_ptr;			/* store running process */
      lock_notify(HARDWARE, CLOCK);		/* send notification */
  } 
  return(1);					/* reenable interrupts */
}

/*===========================================================================*
 *				get_uptime				     *
 *===========================================================================*/
 //n ub BuildMess
PUBLIC clock_t get_uptime()
{
/* Get and return the current clock uptime in ticks. */
  return(realtime);
}

/*===========================================================================*
 *				set_timer				     *
 *===========================================================================*/
PUBLIC void set_timer(tp, exp_time, watchdog)
struct timer *tp;		/* pointer to timer structure */
clock_t exp_time;		/* expiration realtime */
tmr_func_t watchdog;		/* watchdog to be called */
{
/* Insert the new timer in the active timers list. Always update the 
 * next timeout time by setting it to the front of the active list.
 */
  tmrs_settimer(&clock_timers, tp, exp_time, watchdog, NULL);
  next_timeout = clock_timers->tmr_exp_time;
}

/*===========================================================================*
 *				reset_timer				     *
 *===========================================================================*/
PUBLIC void reset_timer(tp)
struct timer *tp;		/* pointer to timer structure */
{
/* The timer pointed to by 'tp' is no longer needed. Remove it from both the
 * active and expired lists. Always update the next timeout time by setting
 * it to the front of the active list.
 */
  tmrs_clrtimer(&clock_timers, tp, NULL);
  next_timeout = (clock_timers == NULL) ? 
	TMR_NEVER : clock_timers->tmr_exp_time;
}

/*===========================================================================*
 *				read_clock				     *
 *===========================================================================*/
 //n reads and returns the current count in the clock chip's countdown register.
PUBLIC unsigned long read_clock()
{
/* Read the counter of channel 0 of the 8253A timer.  This counter counts
 * down at a rate of TIMER_FREQ and restarts at TIMER_COUNT-1 when it
 * reaches zero. A hardware interrupt (clock tick) occurs when the counter
 * gets to zero and restarts its cycle.  
 */
  unsigned count;

  outb(TIMER_MODE, LATCH_COUNT);
  count = inb(TIMER0);
  count |= (inb(TIMER0) << 8);
  
  return count;
}
