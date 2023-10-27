#ifndef __TOOLS
#define __TOOLS

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdint.h>
#include <signal.h>
#include "./scanmem/scanmem.h"

void child_loop()
{
    void* memory = malloc(100 * 1024 * 1024);
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
void terminate_process(int PID)
{
    kill(PID, SIGKILL);
}


void run_scanmem(int PID, int value)
{
    // Initialize the server
    sm_set_backend();
    sm_init();
    sm_backend_exec_cmd("reset");
    
    char command[128];

    // Attach to the process
    snprintf(command, 128, "pid %d", PID);
    sm_backend_exec_cmd(command);
    
    // Make it such that we only search int64
    snprintf(command, 128, "option scan_data_type int");
    sm_backend_exec_cmd(command);
    
    // Scan for values equal to 2
    snprintf(command, 128, "%d", value);
    sm_backend_exec_cmd(command);
    double progress = 0;
    while(progress != 1.0)
    {
        progress = sm_get_scan_progress();
    }
}
#endif
