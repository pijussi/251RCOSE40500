#include <stdio.h>
#include <unistd.h>
#include <sys/syscall.h>
#include <stdlib.h>

#define MY_ENQUEUE_SYSCALL 335
#define MY_DEQUEUE_SYSCALL 336

int main(void) {
    int ret;
    
    printf("Enqueue : 1\n");
    syscall(MY_ENQUEUE_SYSCALL, 1);
    
    printf("Enqueue : 2\n");
    syscall(MY_ENQUEUE_SYSCALL, 2);
    
    printf("Enqueue : 3\n");
    syscall(MY_ENQUEUE_SYSCALL, 3);
    
    printf("Enqueue : 3\n");
    syscall(MY_ENQUEUE_SYSCALL, 3);

    ret = syscall(MY_DEQUEUE_SYSCALL);
    printf("Dequeue : %ld\n", (long)ret);

    ret = syscall(MY_DEQUEUE_SYSCALL);
    printf("Dequeue : %ld\n", (long)ret);

    ret = syscall(MY_DEQUEUE_SYSCALL);
    printf("Dequeue : %ld\n", (long)ret);

    return 0;
}

