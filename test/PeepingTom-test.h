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

void pause_process(int pid)
{
    kill(pid, SIGSTOP);
}
void init_scanmem()
{
    sm_set_backend();
    sm_init();
}
void clear_scanmem()
{

}
unsigned long get_matches64(int PID, int value)
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
    snprintf(command, 128, "option scan_data_type int64");
    sm_backend_exec_cmd(command);
    
    // Scan for values equal to 2
    snprintf(command, 128, "%d", value);
    sm_backend_exec_cmd(command);
    double progress = 0;
    while(progress != 1.0)
    {
        progress = sm_get_scan_progress();
    }
    unsigned long count = sm_get_num_matches();
    return count;
}
unsigned long get_matchesi(int PID, int value)
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
    sm_backend_exec_cmd("reset");
    
    // Scan for values equal to 2
    snprintf(command, 128, "%d", value);
    sm_backend_exec_cmd(command);
    double progress = 0;
    while(progress != 1.0)
    {
        progress = sm_get_scan_progress();
    }
    unsigned long count = sm_get_num_matches();
    return count;
}
void update_values(int PID, int oldvalue, int newvalue)
{
    // Initialize the server
    sm_backend_exec_cmd("reset");
    
    char command[128];

    snprintf(command, 128, "option scan_data_type int64");
    sm_backend_exec_cmd(command);
    
    // Attach to the process
    snprintf(command, 128, "pid %d", PID);
    sm_backend_exec_cmd(command);

    
    // Scan for values equal to 2
    snprintf(command, 128, "%d", oldvalue);
    sm_backend_exec_cmd(command);
    double progress = 0;
    while(progress != 1.0)
    {
        progress = sm_get_scan_progress();
    }

    snprintf(command, 128, "set %d", newvalue);
    sm_backend_exec_cmd(command);
}

void terminate_process(int PID)
{
    kill(PID, SIGKILL);
}
#endif

