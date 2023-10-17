#ifndef __TOOLS
#define __TOOLS

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdint.h>
#include <signal.h>

void child_loop()
{
    while(1){}
}

int create_process()
{
    int pid = fork();
    if(pid == 0){
        child_loop();
    }
    return pid;
}
void kill_process(int pid)
{
    kill(pid, SIGKILL);
}
#endif
