/* Addresses and magic numbers for miscellaneous ports. */

#ifndef _PORTS_H
#define _PORTS_H

#if (CHIP == INTEL)

/* Miscellaneous ports. */
#define PCR		0x65	/* Planar Control Register */
#define PORT_B          0x61	/* I/O port for 8255 port B (kbd, beeper...) */   //n ub  clock_handler

//n 8253A timer hase 3 counters,in 0x40 0x41 0x42 .see clock.c
#define TIMER0          0x40	/* I/O port for timer channel 0 */  //n used in read_clock()	init_clock()
#define TIMER2          0x42	/* I/O port for timer channel 2 */
#define TIMER_MODE      0x43	/* I/O port for timer mode control */   //n used in read_clock()   init_clock()
 

#endif /* (CHIP == INTEL) */

#endif /* _PORTS_H */
