#include <lib.h>
//n ub system calls defined in include/unistd.h,like getpid fork...
PUBLIC int _syscall(who, syscallnr, msgptr)
int who;
int syscallnr;
register message *msgptr;
{
  int status;

  msgptr->m_type = syscallnr;
  status = _sendrec(who, msgptr);//n asm routine __sendrec is defined as 2 version: 86 & 386 version
  if (status != 0) {
	/* 'sendrec' itself failed. */
	/* XXX - strerror doesn't know all the codes */
	msgptr->m_type = status;
  }
  if (msgptr->m_type < 0) {
	errno = -msgptr->m_type;
	return(-1);
  }
  return(msgptr->m_type);
}
