#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/syscall.h>
#include <time.h>
#include <stdbool.h>

#define KU_CPU 339  

int main(int argc, char **argv) {
    if (argc < 4) {
        fprintf(stderr, "Usage: %s <jobTime(s)> <delay(s)> <name>\n", argv[0]);
        return 1;
    }

    int jobTimeSec  = atoi(argv[1]);    
    int delayTime   = atoi(argv[2]);    
    char name[16];
    strncpy(name, argv[3], sizeof(name)-1);
    name[sizeof(name)-1] = '\0';

    time_t t_arrival;
    time(&t_arrival);

    sleep(delayTime);

    time(&t_arrival);	

    printf("\nProcess %s : I will use CPU by %ds.\n",
           name, jobTimeSec);
    fflush(stdout);

    int quanta_left = jobTimeSec * 10;
    int wait_quanta = 0;
    bool started    = false;
    time_t t_first_grant = 0, t_end = 0;

    while (quanta_left > 0) {
        int ret = syscall(KU_CPU, name, quanta_left);
        if (ret != 0) {
            wait_quanta++;
        } else {
            if (!started) {
                time(&t_first_grant);
                started = true;
            }
            quanta_left--;
        }
        usleep(100000);
    }

    syscall(KU_CPU, name, 0);

    time(&t_end);

    int response_time = (int)(t_first_grant - t_arrival);
    int turnaround    = (int)(t_end         - t_arrival);
    int wait_time     = turnaround - jobTimeSec;

    printf("\nProcess %s : Finish! My response time is %ds and My total wait time is %ds.\n",
           name, response_time, wait_time);

    return 0;
}

