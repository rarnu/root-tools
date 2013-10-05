package com.rarnu.huawei.p6.common;

public class FileDefine {

    public static final String BASE_PATH = "/sys/devices/system/cpu/cpu0/cpufreq/";
    public static final String CPU_FREQ = BASE_PATH + "cpuinfo_block_freq";
    public static final String CPU_CORE = BASE_PATH +"scaling_cpu_lock_num";
    public static final String DDR_FREQ = BASE_PATH +"scaling_ddr_block_freq";
    public static final String GPU_FREQ = BASE_PATH +"scaling_gpu_block_freq";
}
