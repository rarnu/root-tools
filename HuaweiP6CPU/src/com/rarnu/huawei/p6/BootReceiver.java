package com.rarnu.huawei.p6;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import com.rarnu.command.RootUtils;
import com.rarnu.huawei.p6.common.FileDefine;
import com.rarnu.huawei.p6.utils.Config;
import com.rarnu.huawei.p6.utils.CpuUtils;
import com.rarnu.huawei.p6.utils.DeviceCheckUtils;

public class BootReceiver extends BroadcastReceiver {
    @Override
    public void onReceive(Context context, Intent intent) {
        String action = intent.getAction();
        if (action.equals(Intent.ACTION_BOOT_COMPLETED)) {
            RootUtils.init(context);
            boolean isP6 = DeviceCheckUtils.isHuaweiP6();
            if (isP6) {
                RootUtils.mountRW();
                CpuUtils.setIntValue(FileDefine.CPU_FREQ, Config.getCpuFreq(context));
                CpuUtils.setIntValue(FileDefine.CPU_CORE, Config.getCpuCore(context));
                CpuUtils.setIntValue(FileDefine.DDR_FREQ, Config.getDdrFreq(context));
                CpuUtils.setIntValue(FileDefine.GPU_FREQ, Config.getGpuFreq(context));
            }
        }
    }


}
