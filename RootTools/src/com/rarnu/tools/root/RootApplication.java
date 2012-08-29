package com.rarnu.tools.root;

import android.app.Application;

public class RootApplication extends Application /* implements MKGeneralListener */ {

	//private static BMapManager mapMgr = null;
	// private static final String mapKey = "7AA204E114A0EFBC0C49171C1D6684C1F3B6C2AA";

//	public BMapManager getMapManager() {
//		if (mapMgr == null) {
//			mapMgr = new BMapManager(this);
//			mapMgr.init(mapKey, this);
//		}
//		return mapMgr;
//	}

	@Override
	public void onCreate() {
//		mapMgr = new BMapManager(this);
//		mapMgr.init(mapKey, this);
		super.onCreate();
		if (!GlobalInstance.DEBUG) {
			Thread.setDefaultUncaughtExceptionHandler(new RootUncaughtExceptionHandler(
					this));
		}
	}

	@Override
	public void onTerminate() {
//		if (mapMgr != null) {
//			mapMgr.destroy();
//			mapMgr = null;
//		}
		super.onTerminate();
	}

}
