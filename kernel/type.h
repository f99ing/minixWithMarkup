#ifndef TYPE_H
#define TYPE_H

typedef _PROTOTYPE( void task_t, (void) );

/* Process table and system property related types. */ 
typedef int proc_nr_t;			/* process table entry number */
typedef short sys_id_t;			/* system process index */
typedef struct {			/* bitmap for system indexes */
  bitchunk_t chunk[BITMAP_CHUNKS(NR_SYS_PROCS)];
} sys_map_t;

//n ub  image[]
struct boot_image {
  proc_nr_t proc_nr;			/* process number to use */
  task_t *initial_pc;			/* start function for tasks */
  int flags;				/* process flags */
  unsigned char quantum;		/* quantum (tick count) */
  int priority;				/* scheduling priority */
  int stksize;				/* stack size for tasks */
  short trap_mask;			/* allowed system call traps */
  bitchunk_t ipc_to;			/* send mask protection */
  long call_mask;			/* system call protection */
  char proc_name[P_NAME_LEN];		/* name in process table */
};

struct memory {
  phys_clicks base;			/* start address of chunk */
  phys_clicks size;			/* size of memory chunk */
};

/* The kernel outputs diagnostic messages in a circular buffer. */
struct kmessages {
  int km_next;				/* next index to write */
  int km_size;				/* current size in buffer */
  char km_buf[KMESS_BUF_SIZE];		/* buffer for messages */
};

struct randomness {
  struct {
	int r_next;				/* next index to write */
	int r_size;				/* number of random elements */
	unsigned short r_buf[RANDOM_ELEMENTS]; /* buffer for random info */
  } bin[RANDOM_SOURCES];
};

#if (CHIP == INTEL)
typedef unsigned reg_t;		/* machine register */

/* The stack frame layout is determined by the software, but for efficiency
 * it is laid out so the assembly code to use it is as simple as possible.
 * 80286 protected mode and all real modes use the same frame, built with
 * 16-bit registers.  Real mode lacks an automatic stack switch, so little
 * is lost by using the 286 frame for it.  The 386 frame differs only in
 * having 32-bit registers and more segment registers.  The same names are
 * used for the larger registers to avoid differences in the code.
 */
struct stackframe_s {           /* proc_ptr points here */  //n ub struct proc
#if _WORD_SIZE == 4
  u16_t gs;                     /* last item pushed by save */
  u16_t fs;                     /*  ^ */
#endif
  u16_t es;                     /*  | */
  u16_t ds;                     /*  | */
  reg_t di;			/* di through cx are not accessed in C */
  reg_t si;			/* order is to match pusha/popa */
  reg_t fp;			/* bp */
  reg_t st;			/* hole for another copy of sp */
  reg_t bx;                     /*  | */
  reg_t dx;                     /*  | */
  reg_t cx;                     /*  | */
  reg_t retreg;			/* ax and above are all pushed by save */
  reg_t retadr;			/* return address for assembly code save() */
  reg_t pc;			/*  ^  last item pushed by interrupt */					//n ub struct proc and in turn in main()
  reg_t cs;                     /*  | */
  reg_t psw;                    /*  | */	//n ub struct proc and in turn in main()  enable_iop
  reg_t sp;                     /*  | */
  reg_t ss;                     /* these are pushed by CPU during interrupt */
};

//n 8 bytes descriptor 
//n ref intel IA32 manual v3 3.4.5 
//n ub seg2phys
struct segdesc_s {		/* segment descriptor for protected mode */
  u16_t limit_low;
  u16_t base_low;
  u8_t base_middle;
  u8_t access;			/* |P|DL|1|X|E|R|A| */
  u8_t granularity;		/* |G|X|0|A|LIMT| */
  u8_t base_high;
};

typedef unsigned long irq_policy_t;	
typedef unsigned long irq_id_t;	

//n ub clock_hook, clock_handler, put_irq_handler, rm_irq_handler
typedef struct irq_hook {
  struct irq_hook *next;		/* next hook in chain */			//n set at put_irq_handler,ub  rm_irq_handler   intr_handle
  int (*handler)(struct irq_hook *);	/* interrupt handler */		//n a function with irq_hook parameter & returns int. for exp:   intr_handle,  set at put_irq_handler
  int irq;				/* IRQ vector number */						//n rm_irq_handler generic_handler set at put_irq_handler
  int id;				/* id of this hook */			//n rm_irq_handler    intr_handle ,set at put_irq_handler  can be 1 10 100 1000...,records the position in its linked list.used to set irq_actids[]
  int proc_nr;				/* NONE if not in use */				//n for clock interrupt handler, this is set in init_clock. ub generic_handler
  irq_id_t notify_id;			/* id to return on interrupt */		//n ub generic_handler
  irq_policy_t policy;			/* bit mask for policy */			//n ub generic_handler
} irq_hook_t;

typedef int (*irq_handler_t)(struct irq_hook *);

#endif /* (CHIP == INTEL) */

#if (CHIP == M68000)
/* M68000 specific types go here. */
#endif /* (CHIP == M68000) */

#endif /* TYPE_H */
