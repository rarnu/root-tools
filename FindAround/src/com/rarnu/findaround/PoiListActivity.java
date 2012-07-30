package com.rarnu.findaround;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.Button;
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
import com.rarnu.findaround.base.BaseActivity;
import com.rarnu.findaround.common.Config;

public class PoiListActivity extends BaseActivity implements OnClickListener,
		OnItemClickListener, MKSearchListener {

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
		lvPoi.setOnItemClickListener(this);

		tvLoading.setVisibility(View.VISIBLE);
	}

	@Override
	protected void mappingComponents() {
		super.mappingComponents();
		tvLoading = (TextView) findViewById(R.id.tvLoading);
		lvPoi = (ListView) findViewById(R.id.lvPoi);
		tvName = (TextView) findViewById(R.id.tvName);
		btnLeft = (Button) findViewById(R.id.btnLeft);
		btnLeft.setText(R.string.back);
		btnLeft.setVisibility(View.VISIBLE);
		tvName.setText(R.string.list_result);
	}

	// [region] events
	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnLeft:
			setResult(RESULT_CANCELED);
			finish();
			break;
		}

	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		GlobalInstance.selectedInfo = GlobalInstance.listPoi.get(position);
		Intent inMap = new Intent(this, MapRouteActivity.class);
		startActivity(inMap);
		
		MKPlanNode nodeStart = new MKPlanNode();
		nodeStart.pt = GlobalInstance.point;
		MKPlanNode nodeEnd = new MKPlanNode();
		nodeEnd.pt = GlobalInstance.selectedInfo.pt;
		if (Config.getMethod(this) == 2) {
			mSearch.walkingSearch(GlobalInstance.city, nodeStart, GlobalInstance.city, nodeEnd);
		} else {
			mSearch.drivingSearch(GlobalInstance.city, nodeStart, GlobalInstance.city, nodeEnd);
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
			lvPoi.setAdapter(adapter);
		}
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
