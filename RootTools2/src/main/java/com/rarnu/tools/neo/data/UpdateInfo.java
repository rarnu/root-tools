package com.rarnu.tools.neo.data;

import android.content.Context;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.os.Handler;
import android.os.Message;
import com.rarnu.tools.neo.api.API;
import org.json.JSONException;
import org.json.JSONObject;

public class UpdateInfo {

    public interface UpdateInfoReadyCallback {
        void onUpdateInfoReady(UpdateInfo info);
    }

    private static UpdateInfo _instance = null;

    public String versionName = "";
    public int versionCode = 0;
    public String description = "";
    public String url = "";

    private boolean isReady = false;
    private UpdateInfoReadyCallback callback = null;

    private Handler hCallback = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (callback != null) {
                callback.onUpdateInfoReady(_instance);
            }
            super.handleMessage(msg);
        }
    };

    private UpdateInfo(UpdateInfoReadyCallback callback) {
        _instance = this;
        this.callback = callback;
        threadInitUpdateInfo();
    }

    private void threadInitUpdateInfo() {
        isReady = false;
        new Thread(new Runnable() {
            @Override
            public void run() {
                String jsonStr = API.getUpdateInfo();
                try {
                    JSONObject json = new JSONObject(jsonStr);
                    versionCode = json.getInt("versionCode");
                    versionName = json.getString("versionName");
                    url = json.getString("url");
                    description = json.getString("description");
                } catch (JSONException e) {

                }
                isReady = true;
                hCallback.sendEmptyMessage(0);
            }
        }).start();
    }

    public boolean isNewVersion(Context ctx) {
        boolean ret = false;
        try {
            PackageManager pm = ctx.getPackageManager();
            PackageInfo pkg = pm.getPackageInfo(ctx.getPackageName(), 0);
            int verCode = pkg.versionCode;
            ret = versionCode > verCode;
        } catch (Exception e) {
        }
        return ret;
    }

    public static UpdateInfo getUpdateInfo(UpdateInfoReadyCallback callback) {
        new UpdateInfo(callback);
        return _instance;
    }

}
