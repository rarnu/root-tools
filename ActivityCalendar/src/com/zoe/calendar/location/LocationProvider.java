package com.zoe.calendar.location;

import android.content.Context;
import android.content.Intent;

import com.baidu.location.BDLocation;
import com.baidu.location.BDLocationListener;
import com.baidu.location.LocationClient;
import com.baidu.location.LocationClientOption;
import com.baidu.mapapi.search.MKAddrInfo;
import com.baidu.mapapi.search.MKBusLineResult;
import com.baidu.mapapi.search.MKDrivingRouteResult;
import com.baidu.mapapi.search.MKPoiResult;
import com.baidu.mapapi.search.MKSearch;
import com.baidu.mapapi.search.MKSearchListener;
import com.baidu.mapapi.search.MKSuggestionResult;
import com.baidu.mapapi.search.MKTransitRouteResult;
import com.baidu.mapapi.search.MKWalkingRouteResult;
import com.zoe.calendar.ActivityApplication;
import com.zoe.calendar.Global;
import com.zoe.calendar.common.Actions;

public class LocationProvider implements BDLocationListener, MKSearchListener {

	Context mContext;
	LocationClient mLocClient;
	MKSearch mSearch;

	public LocationProvider(Context context) {
		mContext = context;
		mLocClient = new LocationClient(mContext);
		mLocClient.registerLocationListener(this);

		mSearch = new MKSearch();
		mSearch.init(ActivityApplication.getInstance().mBMapManager, this);

		LocationClientOption option = new LocationClientOption();
		option.setOpenGps(false);
		option.setCoorType("bd09ll");
		option.setScanSpan(5000);
		option.setPoiExtraInfo(true);
		option.setAddrType("all");
		mLocClient.setLocOption(option);
	}

	public void start() {
		mLocClient.start();
		mLocClient.requestLocation();
	}

	public void close() {
		mLocClient.stop();
	}

	@Override
	public void onReceiveLocation(BDLocation loc) {
		Global.location = loc;
		if (loc != null) {
			if (Global.city == null || Global.city.equals("")) {
				Global.city = loc.getCity();
			}
			mContext.sendBroadcast(new Intent(Actions.ACTION_RECEIVE_LOCATION));
		}

	}

	@Override
	public void onReceivePoi(BDLocation loc) {

	}

	public void searchAddress(String city, String address) {
		mSearch.geocode(address, city);
	}

	@Override
	public void onGetAddrResult(MKAddrInfo res, int error) {
		if (error != 0) {
			return;
		}
		if (res != null && res.geoPt != null) {
			mContext.sendBroadcast(new Intent(Actions.ACTION_RECEIVE_GEOPOI)
					.putExtra("lat", res.geoPt.getLatitudeE6())
					.putExtra("lng", res.geoPt.getLongitudeE6())
					.putExtra("addr", res.strAddr));
		}

	}

	@Override
	public void onGetBusDetailResult(MKBusLineResult arg0, int arg1) {
	}

	@Override
	public void onGetDrivingRouteResult(MKDrivingRouteResult arg0, int arg1) {

	}

	@Override
	public void onGetPoiDetailSearchResult(int arg0, int arg1) {
	}

	@Override
	public void onGetPoiResult(MKPoiResult arg0, int arg1, int arg2) {

	}

	@Override
	public void onGetSuggestionResult(MKSuggestionResult arg0, int arg1) {

	}

	@Override
	public void onGetTransitRouteResult(MKTransitRouteResult arg0, int arg1) {

	}

	@Override
	public void onGetWalkingRouteResult(MKWalkingRouteResult arg0, int arg1) {

	}
}
