package com.rarnu.tools.root.service;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.graphics.Color;
import android.view.View;
import android.widget.ImageView;
import android.widget.Toast;
import com.rarnu.devlib.base.BaseFloatService;
import com.rarnu.devlib.component.DotView;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.Actions;
import com.rarnu.tools.root.common.MemoryInfo;
import com.rarnu.tools.root.common.RTConfig;
import com.rarnu.tools.root.utils.ApkUtils;
import com.rarnu.tools.root.utils.MemoryUtils;
import com.rarnu.utils.UIUtils;

public class FloatWidgetService extends BaseFloatService {

    public static boolean isAlive = false;
    public IntentFilter filterClean = new IntentFilter(Actions.ACTION_CLEAN_MEMORY_FINISH);
    public CleanMemoryReceiver receiverClean = new CleanMemoryReceiver();
    ImageView ivLogo;
    DotView dvClean;
    MemoryInfo lastMemInfo = null;
    boolean isInProcessing = false;

    @Override
    public int getViewResId() {
        return R.layout.view_float_window;
    }

    @Override
    public void initView(View view) {
        ivLogo = (ImageView) view.findViewById(R.id.ivLogo);
        dvClean = (DotView) view.findViewById(R.id.dvClean);
        dvClean.setStartSweep(0F);
        dvClean.setSweepInc(5F);
        dvClean.setPaintColor(Color.WHITE);
        dvClean.setBackgroundFillColor(Color.TRANSPARENT);

    }

    @Override
    public int getX() {
        return GlobalInstance.floatWindowPosX;
    }

    @Override
    public int getY() {
        return GlobalInstance.floatWindowPosY;
    }

    @Override
    public void onPositionChanged(View v, int x, int y) {
        RTConfig.setFloatWindowPosX(this, x);
        RTConfig.setFloatWindowPosY(this, y);
        GlobalInstance.floatWindowPosX = x;
        GlobalInstance.floatWindowPosY = y;
    }

    @Override
    public void onFloatWindowClick() {
        if (!isInProcessing) {
            isInProcessing = true;
            ivLogo.setBackgroundResource(R.drawable.icon_background_36);
            dvClean.setBackgroundSize(UIUtils.dipToPx(32), UIUtils.dipToPx(32));
            dvClean.setVisibility(View.VISIBLE);
            dvClean.start(true);
            lastMemInfo = MemoryUtils.getMemoryInfo();
            Intent inClean = new Intent(this, AutoCleanMemoryService.class);
            inClean.putExtra("message", Actions.ACTION_CLEAN_MEMORY_FINISH);
            startService(inClean);
        }
    }

    @Override
    public void onFloatWindowLongClick() {
        if (!ApkUtils.getTopPackage(this).equals(getPackageName())) {
            if (GlobalInstance.pm == null) {
                GlobalInstance.init(this);
            }
            ApkUtils.openApp(this, getPackageName(), true);
        }
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        isAlive = true;
        RTConfig.initConfig(this);
        registerReceiver(receiverClean, filterClean);
        return super.onStartCommand(intent, flags, startId);
    }

    @Override
    public void onDestroy() {
        isAlive = false;
        unregisterReceiver(receiverClean);
        super.onDestroy();
    }

    public class CleanMemoryReceiver extends BroadcastReceiver {

        @Override
        public void onReceive(Context context, Intent intent) {
            stopService(new Intent(FloatWidgetService.this, AutoCleanMemoryService.class));
            MemoryInfo nowMemInfo = MemoryUtils.getMemoryInfo();
            int releaseMem = -1;
            if (lastMemInfo != null && nowMemInfo != null) {
                releaseMem = Math.abs(nowMemInfo.Free - lastMemInfo.Free);
            }
            dvClean.stop(true);
            dvClean.setVisibility(View.GONE);
            ivLogo.setBackgroundResource(R.drawable.icon36);
            isInProcessing = false;

            Toast.makeText(FloatWidgetService.this, releaseMem == -1 ? getString(R.string.float_clean_error) : getString(R.string.float_clean_fmt, releaseMem), Toast.LENGTH_SHORT).show();

        }
    }


}
