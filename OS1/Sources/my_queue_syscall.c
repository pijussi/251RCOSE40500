#include <linux/kernel.h>
#include <linux/syscalls.h>
#include <linux/linkage.h>

#define QUEUE_SIZE  128

static int my_queue[QUEUE_SIZE];
static int front = 0;
static int rear  = 0;
static int count = 0;


SYSCALL_DEFINE1(oslab_enqueue, int, a)
{
    int i;

    for (i = 0; i < count; i++) {
        if (my_queue[(front + i) % QUEUE_SIZE] == a) {
            printk(KERN_INFO "[Error] - Already existing value");
            return -1;
        }
    }

    if (count == QUEUE_SIZE) {
        printk(KERN_INFO "[Error] - Queue is full\n");
        return -1;
    }

    my_queue[rear] = a;
    rear = (rear + 1) % QUEUE_SIZE;
    count++;

    printk(KERN_INFO "[System call] oslab_enqueue(); -----\n");
    printk(KERN_INFO "Queue Front---------------------\n");

    for (i = 0; i < count; i++) {
        int idx = (front + i) % QUEUE_SIZE;
        printk(KERN_INFO "%d\n", my_queue[idx]);
    }

    printk(KERN_INFO "Queue Rear----------------------\n");

    return 0;
}


SYSCALL_DEFINE0(oslab_dequeue)
{
    int val, i;

    if (count == 0) {
        printk(KERN_INFO "[Error] - Queue is empty\n");
        return -1;
    }

    val = my_queue[front];
    front = (front + 1) % QUEUE_SIZE;
    count--;

    printk(KERN_INFO "[System call] oslab_dequeue(); -----\n");
    printk(KERN_INFO "Queue Front---------------------\n");
    for (i = 0; i < count; i++) {
        int idx = (front + i) % QUEUE_SIZE;
        printk(KERN_INFO "%d\n", my_queue[idx]);
    }
    printk(KERN_INFO "Queue Rear----------------------\n");

    return val;
}
