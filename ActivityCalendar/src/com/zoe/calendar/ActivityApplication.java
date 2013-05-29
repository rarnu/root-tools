package com.zoe.calendar;

import android.app.Application;
import android.util.Log;

import com.baidu.mapapi.BMapManager;
import com.baidu.mapapi.MKGeneralListener;
import com.baidu.mapapi.map.MKEvent;
import com.zoe.calendar.location.LocationProvider;

public class ActivityApplication extends Application implements
		MKGeneralListener {

	private static ActivityApplication mInstance = null;
	public boolean m_bKeyRight = true;
	public BMapManager mBMapManager = null;

	public static final String strKey = "4C0573978FCB30054E907A49AB457D6CC939A0D2";

	@Override
	public void onCreate() {
		super.onCreate();
		mInstance = this;

		Thread.setDefaultUncaughtExceptionHandler(new GlobalExceptionHandler(
				this));

		initEngineManager();

		Global.locProvider = new LocationProvider(this);
		Global.locProvider.start();

	}

	@Override
	public void onTerminate() {
		if (Global.locProvider != null) {
			Global.locProvider.close();
			Global.locProvider = null;
		}
		if (mBMapManager != null) {
			mBMapManager.destroy();
			mBMapManager = null;
		}
		super.onTerminate();
	}

	public void initEngineManager() {
		if (mBMapManager == null) {
			mBMapManager = new BMapManager(this);
		}

		mBMapManager.init(strKey, this);
	}

	public static ActivityApplication getInstance() {
		return mInstance;
	}

	@Override
	public void onGetNetworkState(int iError) {
		switch (iError) {
		case MKEvent.ERROR_NETWORK_CONNECT:
			break;
		case MKEvent.ERROR_NETWORK_DATA:
			break;
		}

	}

	@Override
	public void onGetPermissionState(int iError) {
		switch (iError) {
		case MKEvent.ERROR_PERMISSION_DENIED:
			ActivityApplication.getInstance().m_bKeyRight = false;
			Log.e("onGetPermissionState", "ERROR_PERMISSION_DENIED");
			break;
		}
	}
}