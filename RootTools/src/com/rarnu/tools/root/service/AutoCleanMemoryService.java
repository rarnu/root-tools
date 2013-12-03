package com.rarnu.tools.root.service;

import android.app.Service;
import android.content.Intent;
import android.content.Loader;
import android.os.Handler;
import android.os.IBinder;
import android.os.Message;
import android.util.Log;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.common.Actions;
import com.rarnu.tools.root.common.MemProcessInfo;
import com.rarnu.tools.root.loader.ProcessLoader;
import com.rarnu.tools.root.utils.MemorySpecialList;
import com.rarnu.tools.root.utils.MemoryUtils;

import java.util.ArrayList;
import java.util.List;

public class AutoCleanMemoryService extends Service implements Loader.OnLoadCompleteListener<List<MemProcessInfo>> {

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
                if (finishMessage == null || finishMessage.equals("")) {
                    finishMessage = Actions.ACTION_CLEAN_MEMORY_FINISH;
                }
                sendBroadcast(new Intent(finishMessage));
                isAlive = false;
            }
            super.handleMessage(msg);
        }
    };
    ProcessLoader loader;
    List<MemProcessInfo> listMemProcessAll;
    String finishMessage = Actions.ACTION_CLEAN_MEMORY_FINISH;

    private void initView() {
        loader = new ProcessLoader(this);
        loader.registerListener(0, this);
        listMemProcessAll = new ArrayList<MemProcessInfo>();
    }

    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        flags = START_NOT_STICKY;
        isAlive = true;
        finishMessage = intent.getStringExtra("message");
        initView();
        loader.startLoading();
        return super.onStartCommand(intent, flags, startId);
    }

    @Override
    public void onDestroy() {
        isAlive = false;
        super.onDestroy();
    }

    @Override
    public void onLoadComplete(Loader<List<MemProcessInfo>> loader, List<MemProcessInfo> data) {
        listMemProcessAll.clear();
        if (data != null) {
            listMemProcessAll.addAll(data);
            doClean();
        }
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
                        if (info.appInfo != null) {
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
}
