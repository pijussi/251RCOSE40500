#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/slab.h>
#include <linux/list.h>
#include <linux/spinlock.h>
#include <linux/uaccess.h>
#include <linux/syscalls.h>

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Fizu");
MODULE_DESCRIPTION("ku_cpu syscall—Preemptive Priority with Working prints");

#define IDLE_PID (-1)

struct job_node {
    pid_t            pid;
    char             name[16];
    int              rem_time;   
    int              priority;   
    struct list_head list;
};

static pid_t now_pid    = IDLE_PID;
static char  now_name[16];
static int   now_rem     = 0;
static int   now_prio    = -1;
static LIST_HEAD(queue);
static DEFINE_SPINLOCK(lock);

SYSCALL_DEFINE3(ku_cpu,
    char __user *, uname,
    int,          jobTime,
    int,          priority)
{
    char kname[16];
    pid_t pid = current->pid;
    struct job_node *entry, *next, *iter;
    unsigned long flags;
    bool    in_queue = false;
    bool    preempt  = false;
    int     ret_val  = 1;  

    if (strncpy_from_user(kname, uname, sizeof(kname)) < 0)
        return -EFAULT;
    kname[sizeof(kname)-1] = '\0';

    spin_lock_irqsave(&lock, flags);

    if (jobTime == 0) {
        if (pid == now_pid) {
            printk(KERN_INFO "Process Finished: %s\n", now_name);
            if (list_empty(&queue)) {
                now_pid   = IDLE_PID;
                now_prio  = -1;
                now_name[0]= '\0';
                now_rem   = 0;
            } else {
                next = list_first_entry(&queue, struct job_node, list);
                list_del(&next->list);
                now_pid   = next->pid;
                now_prio  = next->priority;
                now_rem   = next->rem_time;
                strlcpy(now_name, next->name, sizeof(now_name));
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
        now_pid   = pid;
        now_prio  = priority;
        now_rem   = jobTime;
        strlcpy(now_name, kname, sizeof(now_name));

        ret_val = 0;
        printk(KERN_INFO "Working: %s\n", now_name);
        spin_unlock_irqrestore(&lock, flags);
        return ret_val;
    }

    if (pid == now_pid) {
        now_rem = jobTime;

        list_for_each_entry(iter, &queue, list) {
            if (iter->priority < now_prio) {
                preempt = true;
                break;
            }
        }

        if (!preempt) {
            ret_val = 0;
            printk(KERN_INFO "Working: %s\n", now_name);
            spin_unlock_irqrestore(&lock, flags);
            return ret_val;
        }

        entry = kmalloc(sizeof(*entry), GFP_ATOMIC);
        if (entry) {
            entry->pid       = pid;
            entry->rem_time  = now_rem;
            entry->priority  = now_prio;
            strlcpy(entry->name, now_name, sizeof(entry->name));
            INIT_LIST_HEAD(&entry->list);
            {
                struct list_head *pos;
                bool added = false;
                list_for_each(pos, &queue) {
                    struct job_node *e = list_entry(pos, struct job_node, list);
                    if (entry->priority < e->priority) {
                        list_add(&entry->list, pos);
                        added = true;
                        break;
                    }
                }
                if (!added)
                    list_add_tail(&entry->list, &queue);
            }
        }

        next = list_first_entry(&queue, struct job_node, list);
        list_del(&next->list);
        now_pid   = next->pid;
        now_prio  = next->priority;
        now_rem   = next->rem_time;
        strlcpy(now_name, next->name, sizeof(now_name));
        kfree(next);

        ret_val = 0;
        printk(KERN_INFO "Working: %s\n", now_name);
        spin_unlock_irqrestore(&lock, flags);
        return ret_val;
    }

    list_for_each_entry(entry, &queue, list) {
        if (entry->pid == pid) {
            entry->rem_time = jobTime;
            entry->priority = priority;
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
        entry->pid       = pid;
        entry->rem_time  = jobTime;
        entry->priority  = priority;
        strlcpy(entry->name, kname, sizeof(entry->name));
        INIT_LIST_HEAD(&entry->list);
        {
            struct list_head *pos;
            bool added = false;
            list_for_each(pos, &queue) {
                struct job_node *e = list_entry(pos, struct job_node, list);
                if (entry->priority < e->priority) {
                    list_add(&entry->list, pos);
                    added = true;
                    break;
                }
            }
            if (!added)
                list_add_tail(&entry->list, &queue);
        }
    }

    printk(KERN_INFO "Working Denied:%s\n", kname);
    spin_unlock_irqrestore(&lock, flags);
    return ret_val;
}

