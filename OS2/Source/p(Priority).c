#define _GNU_SOURCE             
#define _POSIX_C_SOURCE 200809L 

#include <unistd.h>            
#include <sys/syscall.h>       
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <time.h>               

#define KU_CPU 339  

int main(int argc, char **argv) {
    if (argc < 5) {
        fprintf(stderr,
            "Usage: %s <jobTime(s)> <delay(s)> <name> <priority>\n",
            argv[0]);
        return 1;
    }

    int jobTimeSec = atoi(argv[1]);   
    int delayTime  = atoi(argv[2]);   
    char name[16];
    strncpy(name, argv[3], sizeof(name)-1);
    name[sizeof(name)-1] = '\0';
    int prio = atoi(argv[4]);         

    sleep(delayTime);

    printf("Process %s : I will use CPU by %ds.\n\n",
           name, jobTimeSec);
    fflush(stdout);

    struct timespec t_start, t_first, t_end;
    bool first_grant = false;
    clock_gettime(CLOCK_MONOTONIC, &t_start);

    int quanta_left = jobTimeSec * 10; 
    while (quanta_left > 0) {
        long ret = syscall(KU_CPU, name, quanta_left, prio);
        if (ret == 0) {
            if (!first_grant) {
                clock_gettime(CLOCK_MONOTONIC, &t_first);
                first_grant = true;
            }
            quanta_left--;
            if (quanta_left == 0) {
                clock_gettime(CLOCK_MONOTONIC, &t_end);
                break;
            }
        }
        usleep(100000);
    }

    syscall(KU_CPU, name, 0, prio);

    long elapsed_ns  = (t_end.tv_sec  - t_start.tv_sec ) * 1000000000L
                     + (t_end.tv_nsec - t_start.tv_nsec);
    long response_ns = first_grant
                     ? (t_first.tv_sec  - t_start.tv_sec) * 1000000000L
                       + (t_first.tv_nsec - t_start.tv_nsec)
                     : 0;

    long response_time = response_ns / 1000000000L;
    long wait_time     = (elapsed_ns
                          - (long)jobTimeSec * 1000000000L)
                          / 1000000000L;

    printf("Process %s : Finish! My response time is %lds "
           "and My total wait time is %lds.\n",
           name, response_time, wait_time);

    return 0;
}

