#import "MemoryUtils.h"
#import <sys/types.h>
#import <sys/sysctl.h>
#import <mach/host_info.h>
#import <mach/mach_host.h>
#import <mach/task_info.h>
#import <mach/task.h>

@implementation MemoryInfo

-(id)init {
    if (self) {
        self.total = 0.0;
        self.wired = 0.0;
        self.active = 0.0;
        self.inactive = 0.0;
        self.free = 0.0;
        self.resident = 0.0;
    }
    return self;
}

@end

@implementation MemoryUtils

+(MemoryInfo *)getMemoryInfo {
    int mib[6];
    mib[0] = CTL_HW;
    mib[1] = HW_PAGESIZE;
    int pagesize;
    size_t length;
    length = sizeof (pagesize);
    sysctl (mib, 2, &pagesize, &length, NULL, 0);
    mach_msg_type_number_t count = HOST_VM_INFO_COUNT;
    vm_statistics_data_t vmstat;
    host_statistics(mach_host_self(), HOST_VM_INFO, (host_info_t) &vmstat, &count);
    task_basic_info_64_data_t info;
    unsigned size = sizeof (info);
    task_info (mach_task_self (), TASK_BASIC_INFO_64, (task_info_t) &info, &size);
    double unit = 1024;
    MemoryInfo * minfo = [[MemoryInfo alloc] init];
    minfo.total = (vmstat.wire_count + vmstat.active_count + vmstat.inactive_count + vmstat.free_count) * pagesize / unit;
    minfo.wired = vmstat.wire_count * pagesize / unit;
    minfo.active = vmstat.active_count * pagesize / unit;
    minfo.inactive = vmstat.inactive_count * pagesize / unit;
    minfo.free = vmstat.free_count * pagesize / unit;
    minfo.resident = info.resident_size / unit;
    return minfo;
}

@end
