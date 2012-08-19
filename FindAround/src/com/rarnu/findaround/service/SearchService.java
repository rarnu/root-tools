package com.rarnu.findaround.service;

import java.util.ArrayList;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.ContextWrapper;
import android.content.Intent;
import android.location.Location;
import android.os.Handler;
import android.os.Message;
import android.util.Log;

import com.baidu.mapapi.GeoPoint;
import com.baidu.mapapi.LocationListener;
import com.baidu.mapapi.MKAddrInfo;
import com.baidu.mapapi.MKBusLineResult;
import com.baidu.mapapi.MKDrivingRouteResult;
import com.baidu.mapapi.MKLocationManager;
import com.baidu.mapapi.MKPlanNode;
import com.baidu.mapapi.MKPoiInfo;
import com.baidu.mapapi.MKPoiResult;
import com.baidu.mapapi.MKSearch;
import com.baidu.mapapi.MKSearchListener;
import com.baidu.mapapi.MKSuggestionResult;
import com.baidu.mapapi.MKTransitRouteResult;
import com.baidu.mapapi.MKWalkingRouteResult;
import com.rarnu.findaround.GlobalInstance;
import com.rarnu.findaround.MainApplication;
import com.rarnu.findaround.adapter.PoiInfoEx;
import com.rarnu.findaround.api.BaiduAPI;
import com.rarnu.findaround.common.Config;
import com.rarnu.findaround.common.GeoPointOri;

@SuppressLint("HandlerLeak")
public class SearchService extends ContextWrapper implements MKSearchListener,
		LocationListener {

	MKSearch mSearch;
	MainApplication application;

	public SearchService(Context base, MainApplication application) {
		super(base);
		this.application = application;

		mSearch = new MKSearch();

		mSearch.init(application.getMapManager(), this);

	}

	public void locate() {
		application.getMapManager().getLocationManager()
				.enableProvider(MKLocationManager.MK_NETWORK_PROVIDER);
		application.getMapManager().getLocationManager()
				.enableProvider(MKLocationManager.MK_GPS_PROVIDER);
		application.getMapManager().getLocationManager()
				.requestLocationUpdates(this);

		start();
	}

	public void start() {
		application.getMapManager().start();
	}

	public void stop() {
		application.getMapManager().stop();
	}

	public void searchPoi(String keyword) {
		application.getMapManager().start();
		MKSearch.setPoiPageCapacity(Config.getResultCount(this));
		mSearch.poiSearchNearBy(keyword, GlobalInstance.point,
				Config.getDist(this));
	}

	public void searchWalk(GeoPoint start, GeoPoint end) {
		MKPlanNode nodeStart = new MKPlanNode();
		nodeStart.pt = start;
		MKPlanNode nodeEnd = new MKPlanNode();
		nodeEnd.pt = end;
		mSearch.walkingSearch(GlobalInstance.city, nodeStart,
				GlobalInstance.city, nodeEnd);
	}

	public void searchDrive(GeoPoint start, GeoPoint end) {
		MKPlanNode nodeStart = new MKPlanNode();
		nodeStart.pt = start;
		MKPlanNode nodeEnd = new MKPlanNode();
		nodeEnd.pt = end;
		mSearch.drivingSearch(GlobalInstance.city, nodeStart,
				GlobalInstance.city, nodeEnd);
	}

	@Override
	public void onGetAddrResult(MKAddrInfo arg0, int arg1) {

	}

	@Override
	public void onGetBusDetailResult(MKBusLineResult arg0, int arg1) {

	}

	@Override
	public void onGetDrivingRouteResult(MKDrivingRouteResult res, int error) {
		if (error != 0 || res == null) {
			GlobalInstance.selectedRoute = null;
			return;
		}

		GlobalInstance.selectedRoute = res.getPlan(0).getRoute(0);
		GlobalInstance.routeIndex = -1;
		sendBroadcast(new Intent(MainApplication.ROUTE_FOUND_ACTION));

	}

	@Override
	public void onGetPoiResult(MKPoiResult res, int type, int error) {
		Log.e("PoiListActivity", "onGetPoiResult");
		GlobalInstance.listPoi = null;
		if (error == 0 && res != null) {
			if (res.getAllPoi() != null && res.getAllPoi().size() != 0) {
				GlobalInstance.listPoi = new ArrayList<PoiInfoEx>();
				for (MKPoiInfo info : res.getAllPoi()) {
					PoiInfoEx item = new PoiInfoEx();
					item.info = info;
					item.showButton = false;
					GlobalInstance.listPoi.add(item);
				}
			}
		}
		sendBroadcast(new Intent(MainApplication.POI_FOUND_ACTION));
		application.getMapManager().stop();
	}

	@Override
	public void onGetSuggestionResult(MKSuggestionResult arg0, int arg1) {

	}

	@Override
	public void onGetTransitRouteResult(MKTransitRouteResult arg0, int arg1) {

	}

	@Override
	public void onGetWalkingRouteResult(MKWalkingRouteResult res, int error) {
		if (error != 0 || res == null) {
			GlobalInstance.selectedRoute = null;
			return;
		}

		GlobalInstance.selectedRoute = res.getPlan(0).getRoute(0);
		GlobalInstance.routeIndex = -1;
		sendBroadcast(new Intent(MainApplication.ROUTE_FOUND_ACTION));

	}

	@Override
	public void onLocationChanged(Location location) {
		if (location == null) {
			GlobalInstance.point = null;
		} else {
			GlobalInstance.point = new GeoPoint(
					(int) (location.getLatitude() * 1e6),
					(int) (location.getLongitude() * 1e6));
			GlobalInstance.pointOri = new GeoPointOri(location.getLatitude(),
					location.getLongitude());
			getAddressByGeo(location.getLatitude(), location.getLongitude());
		}

	}

	private void getAddressByGeo(final double lat, final double lng) {

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					sendBroadcast(new Intent(
							MainApplication.ADDRESS_FOUND_ACTION));
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				String addr = BaiduAPI.getAddressViaGeo(lat, lng);
				GlobalInstance.address = addr;
				h.sendEmptyMessage(1);

			}
		}).start();

	}

}
