#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/slab.h>
#include <linux/list.h>
#include <linux/spinlock.h>
#include <linux/uaccess.h>
#include <linux/syscalls.h>

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Fizu");
MODULE_DESCRIPTION("ku_cpu syscall-only FCFS scheduler");

#define IDLE_PID  (-1)

struct job_node {
    pid_t           pid;
    char            name[16];
    struct list_head list;
};

static pid_t          now = IDLE_PID;     
static char           now_name[16] = "";  
static LIST_HEAD(queue);                  
static DEFINE_SPINLOCK(lock);             

SYSCALL_DEFINE2(ku_cpu,
    char __user *, uname,
    int, jobTime)
{
    char kname[16];
    pid_t pid = current->pid;
    struct job_node *entry;
    bool found = false;
    unsigned long flags;

    if (strncpy_from_user(kname, uname, sizeof(kname)) < 0)
        return -EFAULT;
    kname[sizeof(kname)-1] = '\0';

    spin_lock_irqsave(&lock, flags);

    if (now == IDLE_PID) {
        now = pid;
        strlcpy(now_name, kname, sizeof(now_name));
        printk(KERN_INFO "Working: %s\n", now_name);
        spin_unlock_irqrestore(&lock, flags);
        return 0;
    }

    if (now == pid) {
        if (jobTime == 0) {
            printk(KERN_INFO "Process Finished: %s\n", now_name);

            if (list_empty(&queue)) {
                now = IDLE_PID;
                now_name[0] = '\0';
            } else {
                entry = list_first_entry(&queue, struct job_node, list);
                list_del(&entry->list);

                now = entry->pid;
                strlcpy(now_name, entry->name, sizeof(now_name));
                printk(KERN_INFO "Working: %s\n", now_name);
                kfree(entry);
            }
        } else {
            printk(KERN_INFO "Working: %s\n", now_name);
        }

        spin_unlock_irqrestore(&lock, flags);
        return 0;
    }

    list_for_each_entry(entry, &queue, list) {
        if (entry->pid == pid) {
            found = true;
            break;
        }
    }
    if (!found) {
        entry = kmalloc(sizeof(*entry), GFP_ATOMIC);
        if (!entry) {
            spin_unlock_irqrestore(&lock, flags);
            return -ENOMEM;
        }
        entry->pid = pid;
        strlcpy(entry->name, kname, sizeof(entry->name));
        INIT_LIST_HEAD(&entry->list);
        list_add_tail(&entry->list, &queue);
    }

    printk(KERN_INFO "Working Denied: %s\n", kname);
    spin_unlock_irqrestore(&lock, flags);
    return 1;
}
