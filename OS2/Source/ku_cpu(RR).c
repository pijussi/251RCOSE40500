#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/slab.h>
#include <linux/list.h>
#include <linux/spinlock.h>
#include <linux/uaccess.h>
#include <linux/syscalls.h>

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Fizu");
MODULE_DESCRIPTION("ku_cpu syscall—Round Robin scheduler with 1 s quantum");

#define IDLE_PID       (-1)
#define QUANTUM_SLICES 10  

struct job_node {
    pid_t            pid;
    char             name[16];
    int              rem_time;   
    struct list_head list;
};

static pid_t now_pid    = IDLE_PID;
static char  now_name[16];
static int   now_rem     = 0;

static int   quantum_rem = QUANTUM_SLICES;

static LIST_HEAD(queue);
static DEFINE_SPINLOCK(lock);

SYSCALL_DEFINE2(ku_cpu,
    char __user *, uname,
    int,          jobTime)
{
    char kname[16];
    pid_t pid = current->pid;
    struct job_node *entry, *next;
    unsigned long flags;
    bool in_queue = false;

    if (strncpy_from_user(kname, uname, sizeof(kname)) < 0)
        return -EFAULT;
    kname[sizeof(kname)-1] = '\0';

    spin_lock_irqsave(&lock, flags);

    if (jobTime == 0) {
        if (pid == now_pid) {
            printk(KERN_INFO "Process Finished: %s\n", now_name);

            if (list_empty(&queue)) {
                now_pid      = IDLE_PID;
                now_name[0]  = '\0';
                now_rem      = 0;
                quantum_rem  = QUANTUM_SLICES;
            } else {
                next = list_first_entry(&queue, struct job_node, list);
                list_del(&next->list);

                now_pid      = next->pid;
                now_rem      = next->rem_time;
                strlcpy(now_name, next->name, sizeof(now_name));
                quantum_rem  = QUANTUM_SLICES;

                kfree(next);
            }
        } else {
            list_for_each_entry(entry, &queue, list) {
                if (entry->pid == pid) {
                    list_del(&entry->list);
                    kfree(entry);
                    break;
                }
            }
        }
        spin_unlock_irqrestore(&lock, flags);
        return 0;
    }


    if (now_pid == IDLE_PID) {
        now_pid      = pid;
        now_rem      = jobTime;
        strlcpy(now_name, kname, sizeof(now_name));
        quantum_rem  = QUANTUM_SLICES;
        spin_unlock_irqrestore(&lock, flags);
        return 0;
    }

    if (pid == now_pid) {
        now_rem = jobTime;

        printk(KERN_INFO "Working: %s\n", now_name);

        quantum_rem--;

        if (quantum_rem > 0 || list_empty(&queue)) {
            spin_unlock_irqrestore(&lock, flags);
            return 0;
        }

        printk(KERN_INFO "Turn Over ----> %s\n", now_name);

        entry = kmalloc(sizeof(*entry), GFP_ATOMIC);
        if (entry) {
            entry->pid      = pid;
            entry->rem_time = now_rem;
            strlcpy(entry->name, now_name, sizeof(entry->name));
            INIT_LIST_HEAD(&entry->list);
            list_add_tail(&entry->list, &queue);
        }

        next = list_first_entry(&queue, struct job_node, list);
        list_del(&next->list);

        now_pid      = next->pid;
        now_rem      = next->rem_time;
        strlcpy(now_name, next->name, sizeof(now_name));
        quantum_rem  = QUANTUM_SLICES;

        kfree(next);
        spin_unlock_irqrestore(&lock, flags);
        return 0;
    }

    list_for_each_entry(entry, &queue, list) {
        if (entry->pid == pid) {
            entry->rem_time = jobTime;
            in_queue = true;
            break;
        }
    }
    if (!in_queue) {
        entry = kmalloc(sizeof(*entry), GFP_ATOMIC);
        if (!entry) {
            spin_unlock_irqrestore(&lock, flags);
            return -ENOMEM;
        }
        entry->pid      = pid;
        entry->rem_time = jobTime;
        strlcpy(entry->name, kname, sizeof(entry->name));
        INIT_LIST_HEAD(&entry->list);
        list_add_tail(&entry->list, &queue);
    }

    spin_unlock_irqrestore(&lock, flags);
    return 1;  
}

