package com.rarnu.findaround;

import java.util.List;

import android.content.pm.PackageManager;

import com.baidu.mapapi.GeoPoint;
import com.baidu.mapapi.MKPoiInfo;
import com.baidu.mapapi.MKRoute;
import com.rarnu.findaround.adapter.PoiInfoEx;
import com.rarnu.findaround.api.UpdateInfo;
import com.rarnu.findaround.common.GeoPointOri;
import com.rarnu.findaround.service.SearchService;

public class GlobalInstance {

	public static SearchService search = null;
	public static PackageManager pm = null;

	public static List<PoiInfoEx> listPoi = null;
	public static MKPoiInfo selectedInfo = null;
	public static MKRoute selectedRoute = null;
	public static int routeIndex = 0;

	public static GeoPoint point = null;
	public static GeoPointOri pointOri = null;

	public static String address = "";
	public static String city = "";

	public static boolean onTouchMutax = false;
	public static UpdateInfo updateInfo = null;

	// consts
	public static final String NS_ANJUKE = "com.anjuke.android.app";
	public static final String NS_HAOZU = "com.anjuke.android.haozu";
	public static final String NS_BROKER = "com.anjuke.android.newbroker";
	public static final String AS_WELCOME_ANJUKE = ".activity.WelcomeActivity";
	public static final String AS_WELCOME_HAOZU = ".activity.WelcomeActivity";
	public static final String AS_WELCOME_BROKER = ".activity.WelcomeActivity";
}
