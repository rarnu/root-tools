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

import com.rarnu.findaround.adapter.PoiAdapter;
import com.rarnu.findaround.api.BaiduAPI;
import com.rarnu.findaround.base.BaseActivity;
import com.rarnu.findaround.common.GeoPointOri;
import com.rarnu.findaround.common.NetFiles;
import com.rarnu.findaround.common.UIUtils;
import com.rarnu.findaround.comp.MapHead;

public class PoiListActivity extends BaseActivity implements OnClickListener,
		OnItemClickListener {

	ListView lvPoi;
	TextView tvLoading;
	PoiAdapter adapter;
	String keyword;

	boolean loading = true;

	// [region] lift circle
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.poi_list);
		init();
		keyword = getIntent().getStringExtra("keyword");

	}

	@Override
	protected void onResume() {

		registerReceiver(myreceiver, mapFilter);
		registerReceiver(searchReceiver, searchFilter);
		if (loading) {
			loading = false;
			GlobalInstance.search.searchPoi(keyword);
		}

		super.onResume();

	}

	@Override
	protected void onPause() {
		GlobalInstance.search.stop();
		unregisterReceiver(myreceiver);
		unregisterReceiver(searchReceiver);
		
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
			GlobalInstance.selectedInfo = null;
		} else {
			GlobalInstance.selectedInfo = GlobalInstance.listPoi.get(position - 1);
		}
		
		Intent inMap = new Intent(this, MapRouteActivity.class);
		startActivity(inMap);

	}

	// [/region]

	// [region] map callback

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

	// [/region]

	class MapReceiver extends BroadcastReceiver {

		@Override
		public void onReceive(Context context, Intent intent) {
			tvLoading.setVisibility(View.GONE);
			Toast.makeText(context, R.string.network_error, Toast.LENGTH_LONG)
					.show();
		}

	}

	class SearchReceiver extends BroadcastReceiver {
		@Override
		public void onReceive(Context context, Intent intent) {

			tvLoading.setVisibility(View.GONE);

			lvPoi.setAdapter(null);
			if (GlobalInstance.listPoi == null) {
				Toast.makeText(PoiListActivity.this, R.string.no_result,
						Toast.LENGTH_LONG).show();
				finish();
				return;
			}

			if (GlobalInstance.listPoi.size() != 0) {
				adapter = new PoiAdapter(getLayoutInflater(),
						GlobalInstance.listPoi);
				initMapPos();
				lvPoi.setAdapter(adapter);
			}
		}
	}

	private MapReceiver myreceiver = new MapReceiver();
	private IntentFilter mapFilter = new IntentFilter(
			MainApplication.NETWORK_ERROR_ACTION);

	private SearchReceiver searchReceiver = new SearchReceiver();
	private IntentFilter searchFilter = new IntentFilter(
			MainApplication.POI_FOUND_ACTION);
}
