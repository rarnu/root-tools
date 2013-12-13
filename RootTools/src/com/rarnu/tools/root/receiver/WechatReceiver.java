package com.rarnu.tools.root.receiver;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import com.rarnu.tools.root.utils.ShareUtils;

public class WechatReceiver extends BroadcastReceiver {

    @Override
    public void onReceive(Context context, Intent intent) {
        ShareUtils.registerToWechat(context);
    }
}
