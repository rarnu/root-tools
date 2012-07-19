package com.rarnu.findaround;

import java.util.List;

import com.baidu.mapapi.MKPoiInfo;
import com.baidu.mapapi.MKRoute;

public class GlobalInstance {

	public static List<MKPoiInfo> listPoi = null;
	public static MKPoiInfo selectedInfo = null;
	public static MKRoute selectedRoute = null;
	public static int routeIndex = 0;
}
