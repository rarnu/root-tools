package com.rarnu.tools.root.service;

import android.content.Intent;
import android.content.Loader;
import android.graphics.Color;
import android.os.Handler;
import android.os.Message;
import android.util.Log;
import android.view.View;
import android.widget.ImageView;
import android.widget.Toast;
import com.rarnu.devlib.base.BaseFloatService;
import com.rarnu.devlib.component.DotView;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.MemProcessInfo;
import com.rarnu.tools.root.common.MemoryInfo;
import com.rarnu.tools.root.common.RTConfig;
import com.rarnu.tools.root.loader.ProcessLoader;
import com.rarnu.tools.root.utils.ApkUtils;
import com.rarnu.tools.root.utils.MemorySpecialList;
import com.rarnu.tools.root.utils.MemoryUtils;
import com.rarnu.utils.UIUtils;

import java.util.ArrayList;
import java.util.List;

public class FloatWidgetService extends BaseFloatService implements Loader.OnLoadCompleteListener<List<MemProcessInfo>> {

    public static boolean isAlive = false;
    final Handler hKillProc = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                doDropCacheT();
            }
            super.handleMessage(msg);
        }
    };
    final Handler hDropCache = new Handler() {

        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
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
            super.handleMessage(msg);
        }
    };
    ImageView ivLogo;
    DotView dvClean;
    ProcessLoader loader;
    List<MemProcessInfo> listMemProcessAll;
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
        loader = new ProcessLoader(this);
        loader.registerListener(0, this);
        listMemProcessAll = new ArrayList<MemProcessInfo>();

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
            loader.startLoading();

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
        return super.onStartCommand(intent, flags, startId);
    }

    @Override
    public void onDestroy() {
        isAlive = false;
        super.onDestroy();
    }

    private void doClean() {

        if (GlobalInstance.killProcessBeforeClean) {
            doKillProcT();
        } else {
            doDropCacheT();
        }
    }

    private void doKillProcT() {

        new Thread(new Runnable() {
            @Override
            public void run() {

                List<MemProcessInfo> lstTmp = new ArrayList<MemProcessInfo>();
                lstTmp.addAll(listMemProcessAll);

                if (lstTmp != null && lstTmp.size() != 0) {
                    for (MemProcessInfo info : lstTmp) {
                        // only kill the user applications
                        if (info.appInfo != null) {
                            // exclude list
                            if (MemorySpecialList.inExcludeList(info.NAME) == -1) {
                                Log.e("KILL", info.NAME);
                                MemoryUtils.killProcess(info.PID);
                            }
                        }
                    }
                }
                hKillProc.sendEmptyMessage(1);
            }
        }).start();

    }

    private void doDropCacheT() {
        new Thread(new Runnable() {

            @Override
            public void run() {
                MemoryUtils.dropCache();
                hDropCache.sendEmptyMessage(1);

            }
        }).start();
    }

    @Override
    public void onLoadComplete(Loader<List<MemProcessInfo>> loader, List<MemProcessInfo> data) {
        listMemProcessAll.clear();
        if (data != null) {
            listMemProcessAll.addAll(data);
            doClean();
        }
    }
}
