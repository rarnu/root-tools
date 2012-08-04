package com.rarnu.findaround;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AbsListView;
import android.widget.AbsListView.LayoutParams;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;

import com.baidu.mapapi.MKAddrInfo;
import com.baidu.mapapi.MKBusLineResult;
import com.baidu.mapapi.MKDrivingRouteResult;
import com.baidu.mapapi.MKPlanNode;
import com.baidu.mapapi.MKPoiResult;
import com.baidu.mapapi.MKSearch;
import com.baidu.mapapi.MKSearchListener;
import com.baidu.mapapi.MKSuggestionResult;
import com.baidu.mapapi.MKTransitRouteResult;
import com.baidu.mapapi.MKWalkingRouteResult;
import com.rarnu.findaround.adapter.PoiAdapter;
import com.rarnu.findaround.api.BaiduAPI;
import com.rarnu.findaround.base.BaseActivity;
import com.rarnu.findaround.common.Config;
import com.rarnu.findaround.common.GeoPointOri;
import com.rarnu.findaround.common.NetFiles;
import com.rarnu.findaround.common.SIMUtils;
import com.rarnu.findaround.common.UIUtils;
import com.rarnu.findaround.comp.MapHead;

public class PoiListActivity extends BaseActivity implements
		OnClickListener, OnItemClickListener, MKSearchListener {

	ListView lvPoi;
	TextView tvLoading;
	PoiAdapter adapter;
	MKSearch mSearch;
	String keyword;

	boolean loading = true;

	// [region] lift circle
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.poi_list);
		init();
		keyword = getIntent().getStringExtra("keyword");
		MainApplication app = (MainApplication) getApplication();
		app.getMapManager().start();
		mSearch = new MKSearch();
		MKSearch.setPoiPageCapacity(Config.getResultCount(this));
		mSearch.init(app.getMapManager(), this);

	}

	@Override
	protected void onResume() {

		MainApplication app = (MainApplication) getApplication();
		app.getMapManager().start();

		if (loading) {
			loading = false;

			mSearch.poiSearchNearBy(keyword, GlobalInstance.point,
					Config.getDist(this));
			if (!SIMUtils.isSimCardReady(this)) {
				doCellGetPoi(keyword, GlobalInstance.pointOri,
						Config.getDist(this));
			}
		}

		super.onResume();
		registerReceiver(myreceiver, mapFilter);
	}

	@Override
	protected void onPause() {
		MainApplication app = (MainApplication) getApplication();
		app.getMapManager().stop();
		unregisterReceiver(myreceiver);
		super.onPause();

	}

	// [/region]

	@Override
	protected void init() {
		super.init();
		btnLeft.setOnClickListener(this);
		tvName.setOnClickListener(this);
		lvPoi.setOnItemClickListener(this);
		tvLoading.setVisibility(View.VISIBLE);
	}

	@Override
	protected void mappingComponents() {
		super.mappingComponents();
		tvLoading = (TextView) findViewById(R.id.tvLoading);
		lvPoi = (ListView) findViewById(R.id.lvPoi);
		tvName.setText(R.string.list_result);
	}

	private void initMapPos() {

		String url = "http://api.map.baidu.com/staticimage?";
		url += String.format("center=%f,%f&width=%d&height=%d&zoom=%d",
				GlobalInstance.pointOri.longitude,
				GlobalInstance.pointOri.latitude,
				UIUtils.pxToDip(UIUtils.getWidth()), 200, 16);

		String markers = "";
		double lat, lng;
		for (int i = 0; i < GlobalInstance.listPoi.size(); i++) {
			lat = (double) (((double) GlobalInstance.listPoi.get(i).pt
					.getLatitudeE6()) / 1e6);
			lng = (double) (((double) GlobalInstance.listPoi.get(i).pt
					.getLongitudeE6()) / 1e6);
			markers += String.format("%f,%f|", lng, lat);
		}
		url += "&markers=" + markers;
		url += "&markerStyles=s,%20,0xff0000";

		MapHead imgMap = new MapHead(this);
		imgMap.setLayoutParams(new AbsListView.LayoutParams(
				LayoutParams.MATCH_PARENT, 0));
		Log.e("BAIDU", url);
		NetFiles.doDownloadImageT(this, url, "tmp.jpg", imgMap, lvPoi);
		lvPoi.addHeaderView(imgMap);
	}

	// [region] events
	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.tvName:
		case R.id.btnLeft:

			finish();
			break;
		}

	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		if (position == 0) {
			// TODO: click map
			return;
		}
		GlobalInstance.selectedInfo = GlobalInstance.listPoi.get(position - 1);
		Intent inMap = new Intent(this, MapRouteActivity.class);
		startActivity(inMap);

		MKPlanNode nodeStart = new MKPlanNode();
		nodeStart.pt = GlobalInstance.point;
		MKPlanNode nodeEnd = new MKPlanNode();
		nodeEnd.pt = GlobalInstance.selectedInfo.pt;
		if (Config.getMethod(this) == 2) {
			mSearch.walkingSearch(GlobalInstance.city, nodeStart,
					GlobalInstance.city, nodeEnd);
		} else {
			mSearch.drivingSearch(GlobalInstance.city, nodeStart,
					GlobalInstance.city, nodeEnd);
		}
	}

	// [/region]

	// [region] map callback
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
		tvLoading.setVisibility(View.GONE);
		GlobalInstance.listPoi = null;
		lvPoi.setAdapter(null);

		if (error != 0 || res == null) {
			Toast.makeText(this, R.string.no_result, Toast.LENGTH_LONG).show();
			finish();
			return;
		}
		if (res.getAllPoi() != null && res.getAllPoi().size() != 0) {
			GlobalInstance.listPoi = res.getAllPoi();

		}
		if (GlobalInstance.listPoi != null
				&& GlobalInstance.listPoi.size() != 0) {
			adapter = new PoiAdapter(getLayoutInflater(),
					GlobalInstance.listPoi);
			initMapPos();
			lvPoi.setAdapter(adapter);

		}
	}

	public void doCellGetPoi(final String keyword, final GeoPointOri geo,
			final int radius) {
		final Handler h = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					tvLoading.setVisibility(View.GONE);
					if (GlobalInstance.listPoi != null
							&& GlobalInstance.listPoi.size() != 0) {
						adapter = new PoiAdapter(getLayoutInflater(),
								GlobalInstance.listPoi);
						lvPoi.setAdapter(adapter);
					}
				}
				super.handleMessage(msg);
			}

		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				GlobalInstance.listPoi = BaiduAPI.getPoiListViaGeo(
						geo.latitude, geo.longitude, radius, keyword);
				h.sendEmptyMessage(1);

			}
		}).start();
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

	// [/region]

	class MapReceiver extends BroadcastReceiver {

		@Override
		public void onReceive(Context context, Intent intent) {
			tvLoading.setVisibility(View.GONE);
			Toast.makeText(context, R.string.network_error, Toast.LENGTH_LONG)
					.show();
		}

	}

	private MapReceiver myreceiver = new MapReceiver();
	private IntentFilter mapFilter = new IntentFilter(
			MainApplication.NETWORK_ERROR_ACTION);
}
