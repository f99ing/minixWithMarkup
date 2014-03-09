#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

_PROTOTYPE(int main, (void));

int main()
{
	printf( "Pid: %d",getpid() );//  system calls are listed in include/unistd.h
	exit(0);
}
