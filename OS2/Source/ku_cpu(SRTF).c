#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/slab.h>
#include <linux/list.h>
#include <linux/spinlock.h>
#include <linux/uaccess.h>
#include <linux/syscalls.h>

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Fizu");
MODULE_DESCRIPTION("ku_cpu syscall-only SRTF scheduler");

#define IDLE_PID (-1)

struct job_node {
    pid_t           pid;
    char            name[16];
    int             rem_time;       
    struct list_head list;
};

static pid_t          now = IDLE_PID;       
static char           now_name[16] = "";  
static int            now_rem = 0;          
static LIST_HEAD(queue);                    
static DEFINE_SPINLOCK(lock);               

SYSCALL_DEFINE2(ku_cpu,
    char __user *, uname,
    int,          jobTime)
{
    char kname[16];
    pid_t pid = current->pid;
    struct job_node *entry, *best;
    bool found = false;
    unsigned long flags;

    if (strncpy_from_user(kname, uname, sizeof(kname)) < 0)
        return -EFAULT;
    kname[sizeof(kname)-1] = '\0';

    spin_lock_irqsave(&lock, flags);

    if (now == IDLE_PID) {
        now = pid;
        strlcpy(now_name, kname, sizeof(now_name));
        now_rem = jobTime;
        printk(KERN_INFO "Working: %s\n", now_name);
        spin_unlock_irqrestore(&lock, flags);
        return 0;
    }

    if (now == pid) {
        now_rem = jobTime;
        if (jobTime == 0) {
            printk(KERN_INFO "Process Finished: %s\n", now_name);
            
            if (list_empty(&queue)) {
                now = IDLE_PID;
                now_name[0] = '\0';
                now_rem = 0;
            } else {
                best = list_first_entry(&queue, struct job_node, list);
                list_for_each_entry(entry, &queue, list) {
                    if (entry->rem_time < best->rem_time)
                        best = entry;
                }
                list_del(&best->list);
                now = best->pid;
                strlcpy(now_name, best->name, sizeof(now_name));
                now_rem = best->rem_time;
                printk(KERN_INFO "Working: %s\n", now_name);
                kfree(best);
            }
        } else {
            printk(KERN_INFO "Working: %s\n", now_name);
        }
        spin_unlock_irqrestore(&lock, flags);
        return 0;
    }

    list_for_each_entry(entry, &queue, list) {
        if (entry->pid == pid) {
            entry->rem_time = jobTime;
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
        entry->rem_time = jobTime;
        INIT_LIST_HEAD(&entry->list);
        list_add_tail(&entry->list, &queue);
    }

    if (jobTime < now_rem) {
        struct job_node *curr_node = kmalloc(sizeof(*curr_node), GFP_ATOMIC);
        if (curr_node) {
            curr_node->pid = now;
            strlcpy(curr_node->name, now_name, sizeof(curr_node->name));
            curr_node->rem_time = now_rem;
            INIT_LIST_HEAD(&curr_node->list);
            list_add_tail(&curr_node->list, &queue);
        }
        now = pid;
        strlcpy(now_name, kname, sizeof(now_name));
        now_rem = jobTime;
        list_for_each_entry(entry, &queue, list) {
            if (entry->pid == pid) {
                list_del(&entry->list);
                kfree(entry);
                break;
            }
        }
        printk(KERN_INFO "Working: %s\n", now_name);
        spin_unlock_irqrestore(&lock, flags);
        return 0;
    }

    printk(KERN_INFO "Working Denied: %s\n", kname);
    spin_unlock_irqrestore(&lock, flags);
    return 1;
}
