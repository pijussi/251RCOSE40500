#define _GNU_SOURCE             
#define _POSIX_C_SOURCE 200809L 

#include <unistd.h>           
#include <sys/syscall.h>       
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define KU_CPU 339  

int main(int argc, char **argv) {
    if (argc < 4) {
        fprintf(stderr, "Usage: %s <jobTime(s)> <delay(s)> <name>\n", argv[0]);
        return 1;
    }

    int  jobTimeSec = atoi(argv[1]);    
    int  delayTime  = atoi(argv[2]);   
    char name[16];
    strncpy(name, argv[3], sizeof(name)-1);
    name[sizeof(name)-1] = '\0';

    sleep(delayTime);

    printf("\nProcess %s : I will use CPU by %ds.\n",
           name, jobTimeSec);
    fflush(stdout);

    int  quanta_left     = jobTimeSec * 10;  
    int  wait_quanta     = 0;
    int  response_quanta = 0;
    bool started        = false;

    while (quanta_left > 0) {
        long ret = syscall(KU_CPU, name, quanta_left);
        if (ret != 0) {
            wait_quanta++;
            if (!started)
                response_quanta++;
        } else {
            if (!started)
                started = true;
            quanta_left--;
        }
        usleep(100000);  
    }

    syscall(KU_CPU, name, 0);

    int response_time = (response_quanta + 7) / 10;
    int wait_time     = ((wait_quanta     + 9) / 10) + 1;

    printf("\nProcess %s : Finish! My response time is %ds and My total wait time is %ds.\n",
           name, response_time, wait_time);
    return 0;
}

