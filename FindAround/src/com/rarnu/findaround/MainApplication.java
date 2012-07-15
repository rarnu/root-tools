package com.rarnu.findaround;

import android.app.Application;
import android.content.Intent;
import android.util.Log;

import com.baidu.mapapi.BMapManager;
import com.baidu.mapapi.MKGeneralListener;

public class MainApplication extends Application implements MKGeneralListener {

	private static BMapManager mapMgr = null;
	private static final String mapKey = "7AA204E114A0EFBC0C49171C1D6684C1F3B6C2AA";
	public static final String NETWORK_ERROR_ACTION = "map.anjuke.error";
	
	public BMapManager getMapManager() {
		if (mapMgr == null) {
			mapMgr = new BMapManager(this);
			mapMgr.init(mapKey, this);
		}
		return mapMgr;
	}
	
	@Override
	public void onCreate() {
		mapMgr = new BMapManager(this);
		mapMgr.init(mapKey, this);
		super.onCreate();
	}
	
	@Override
	public void onTerminate() {
		if (mapMgr != null) {
			mapMgr.destroy();
			mapMgr = null;
		}
		super.onTerminate();
	}

	@Override
	public void onGetNetworkState(int error) {
		Log.e("onGetNetworkState", "error");
		sendBroadcast(new Intent(NETWORK_ERROR_ACTION));
	}

	@Override
	public void onGetPermissionState(int error) {
		Log.e("onGetPermissionState", "error");
		
	}
}
