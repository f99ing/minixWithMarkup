/* This file contains routines for initializing the 8259 interrupt controller:
 *	put_irq_handler: register an interrupt handler
 *	rm_irq_handler: deregister an interrupt handler
 *	intr_handle:	handle a hardware interrupt
 *	intr_init:	initialize the interrupt controller(s)
 */

#include "kernel.h"
#include "proc.h"
#include <minix/com.h>

/*

ICW1  (8259 Initialisation Command Word One)

REF http://www.thesatya.com/8259.html

*/
#define ICW1_AT         0x11	/* edge triggered, cascade, need ICW4 */
#define ICW1_PC         0x13	/* edge triggered, no cascade, need ICW4 */
#define ICW1_PS         0x19	/* level triggered, cascade, need ICW4 */
#define ICW4_AT_SLAVE   0x01	/* not SFNM, not buffered, normal EOI, 8086 */
#define ICW4_AT_MASTER  0x05	/* not SFNM, not buffered, normal EOI, 8086 */
#define ICW4_PC_SLAVE   0x09	/* not SFNM, buffered, normal EOI, 8086 */
#define ICW4_PC_MASTER  0x0D	/* not SFNM, buffered, normal EOI, 8086 */

#if _WORD_SIZE == 2
typedef _PROTOTYPE( void (*vecaddr_t), (void) );

FORWARD _PROTOTYPE( void set_vec, (int vec_nr, vecaddr_t addr) );

PRIVATE vecaddr_t int_vec[] = {
  int00, int01, int02, int03, int04, int05, int06, int07,
};

PRIVATE vecaddr_t irq_vec[] = {
  hwint00, hwint01, hwint02, hwint03, hwint04, hwint05, hwint06, hwint07,
  hwint08, hwint09, hwint10, hwint11, hwint12, hwint13, hwint14, hwint15,
};
#else
#define set_vec(nr, addr)	((void)0)
#endif

/*===========================================================================*
 *				intr_init				     *
 *===========================================================================*/
 //n mine =1 or 0,1 for minix ,0 for return to BIOS
 //n  For each of the two i8259 chips, there is a control port that sets the mode and another port that receives a sequence of four bytes in the initialization sequence. 
PUBLIC void intr_init(mine)
int mine;
{
/* Initialize the 8259s, finishing with all interrupts disabled.  This is
 * only done in protected mode, in real mode we don't touch the 8259s, but
 * use the BIOS locations instead.  The flag "mine" is set if the 8259s are
 * to be programmed for MINIX, or to be reset to what the BIOS expects.
 */
  int i;

  intr_disable();//n In lib/i386/misc/io_intr.s in asm code. just 'cli ret'.?

  if (machine.protected) {//n machine.protected is set at the beginning of cstart()
      /* The AT and newer PS/2 have two interrupt controllers, one master,
       * one slaved at IRQ 2.  (We don't have to deal with the PC that
       * has just one controller, because it must run in real mode.)
       */
	   //n some ints are masked, what are they and why?
      outb(INT_CTL, machine.ps_mca ? ICW1_PS : ICW1_AT);
      outb(INT_CTLMASK, mine ? IRQ0_VECTOR : BIOS_IRQ0_VEC);
							/* ICW2 for master */
      outb(INT_CTLMASK, (1 << CASCADE_IRQ));		/* ICW3 tells slaves */
      outb(INT_CTLMASK, ICW4_AT_MASTER);
      outb(INT_CTLMASK, ~(1 << CASCADE_IRQ));		/* IRQ 0-7 mask */
      outb(INT2_CTL, machine.ps_mca ? ICW1_PS : ICW1_AT);
      outb(INT2_CTLMASK, mine ? IRQ8_VECTOR : BIOS_IRQ8_VEC);
							/* ICW2 for slave */
      outb(INT2_CTLMASK, CASCADE_IRQ);		/* ICW3 is slave nr */
      outb(INT2_CTLMASK, ICW4_AT_SLAVE);
      outb(INT2_CTLMASK, ~0);				/* IRQ 8-15 mask */

      /* Copy the BIOS vectors from the BIOS to the Minix location, so we
       * can still make BIOS calls without reprogramming the i8259s.
       */
#if IRQ0_VECTOR != BIOS_IRQ0_VEC
      phys_copy(BIOS_VECTOR(0) * 4L, VECTOR(0) * 4L, 8 * 4L);
#endif
#if IRQ8_VECTOR != BIOS_IRQ8_VEC
      phys_copy(BIOS_VECTOR(8) * 4L, VECTOR(8) * 4L, 8 * 4L);
#endif
  } else {
      /* Use the BIOS interrupt vectors in real mode.  We only reprogram the
       * exceptions here, the interrupt vectors are reprogrammed on demand.
       * SYS_VECTOR is the Minix system call for message passing.
       */
      for (i = 0; i < 8; i++) set_vec(i, int_vec[i]);
      set_vec(SYS_VECTOR, s_call);
  }
}

/*===========================================================================*
 *				put_irq_handler				     *
 *===========================================================================*/
//n exp usage : in init_clock() 
PUBLIC void put_irq_handler(hook, irq, handler)
irq_hook_t *hook;
int irq;
irq_handler_t handler;
{
/* Register an interrupt handler. */
  int id;
  irq_hook_t **line;

  if (irq < 0 || irq >= NR_IRQ_VECTORS)
      panic("invalid call to put_irq_handler", irq);

  line = &irq_handlers[irq];
  id = 1;
  while (*line != NULL) {
      if (hook == *line) return;	/* extra initialization */
      line = &(*line)->next;
      id <<= 1;
  }
  if (id == 0) panic("Too many handlers for irq", irq);

  hook->next = NULL;
  hook->handler = handler;
  hook->irq = irq;
  hook->id = id;
  *line = hook;

  irq_use |= 1 << irq;
}

/*===========================================================================*
 *				rm_irq_handler				     *
 *===========================================================================*/
PUBLIC void rm_irq_handler(hook)
irq_hook_t *hook;
{
/* Unregister an interrupt handler. */
  int irq = hook->irq; 
  int id = hook->id;
  irq_hook_t **line;

  if (irq < 0 || irq >= NR_IRQ_VECTORS) 
      panic("invalid call to rm_irq_handler", irq);

  line = &irq_handlers[irq];
  while (*line != NULL) {
      if ((*line)->id == id) {
          (*line) = (*line)->next;
          if (! irq_handlers[irq]) irq_use &= ~(1 << irq);
          return;
      }
      line = &(*line)->next;
  }
  /* When the handler is not found, normally return here. */
}

/*===========================================================================*
 *				intr_handle				     *
 *===========================================================================*/
 /*
 scans a linked list of structures that hold, among other things, addresses of functions to be called to handle an interrupt for a device, and the process numbers of the device drivers. It is a linked list because a single IRQ line may be shared with several devices. The handler for each device is supposed to test whether its device actually needs service. Of course, this step is not necessary for an IRQ such as the clock interrupt, IRQ 0, which is hard wired to the chip that generates clock signals with no possibility of any other device triggering this IRQ.

_Intr_handle is hardware dependent

The handler code is intended to be written so it can return quickly. 

If there is no work to be done or the interrupt service is completed immediately, the handler returns TRUE. 

 A handler may perform an operation like reading data from an input device and transferring the data to a buffer where it can be accessed when the corresponding driver has its next chance to run. The handler may then cause a message to be sent to its device driver, which in turn causes the device driver to be scheduled to run as a normal process. 
 
 If the work is not complete, the handler returns FALSE. 
 
 */
PUBLIC void intr_handle(hook)
irq_hook_t *hook;
{
/* Call the interrupt handlers for an interrupt with the given hook list.
 * The assembly part of the handler has already masked the IRQ, reenabled the
 * controller(s) and enabled interrupts.
 */

  /* Call list of handlers for an IRQ. */
  while (hook != NULL) {
      /* For each handler in the list, mark it active by setting its ID bit,
       * call the function, and unmark it if the function returns true.
       */
      irq_actids[hook->irq] |= hook->id;
      if ((*hook->handler)(hook)) irq_actids[hook->irq] &= ~hook->id;
      hook = hook->next;
  }

  /* The assembly code will now disable interrupts, unmask the IRQ if and only
   * if all active ID bits are cleared, and restart a process.
   */
}

#if _WORD_SIZE == 2
/*===========================================================================*
 *				set_vec                                      *
 *===========================================================================*/
PRIVATE void set_vec(vec_nr, addr)
int vec_nr;			/* which vector */
vecaddr_t addr;			/* where to start */
{
/* Set up a real mode interrupt vector. */

  u16_t vec[2];

  /* Build the vector in the array 'vec'. */
  vec[0] = (u16_t) addr;
  vec[1] = (u16_t) physb_to_hclick(code_base);

  /* Copy the vector into place. */
  phys_copy(vir2phys(vec), vec_nr * 4L, 4L);
}
#endif /* _WORD_SIZE == 2 */
