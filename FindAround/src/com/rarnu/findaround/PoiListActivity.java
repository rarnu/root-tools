package com.rarnu.findaround;

import android.annotation.SuppressLint;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.Button;
import android.widget.ListView;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.Toast;

import com.baidu.mapapi.MapView;
import com.baidu.mapapi.OverlayItem;
import com.rarnu.findaround.adapter.PoiAdapter;
import com.rarnu.findaround.adapter.PoiInfoEx;
import com.rarnu.findaround.api.BaiduAPI;
import com.rarnu.findaround.base.BaseMapActivity;
import com.rarnu.findaround.common.GeoPointOri;
import com.rarnu.findaround.common.UIUtils;
import com.rarnu.findaround.map.NoTouchOverlay;

@SuppressLint("HandlerLeak")
public class PoiListActivity extends BaseMapActivity implements
		OnClickListener, OnItemClickListener {

	// private static final int POPUP_ID = 100001;

	MapView mvMap;
	NoTouchOverlay noTouchOverlay;
	ListView lvPoi;
	TextView tvLoading;
	PoiAdapter adapter;
	String keyword;
	RelativeLayout layMapBottom;
	RelativeLayout layGeoItem;
	Button btnReturnList;
	TextView tvPoiName, tvPoiAddress;
	Button btnPoiDistance;

	Drawable marker, markerGreen;
	boolean mapMode = false;
	// PopupView popup;

	boolean loading = true;

	Handler hPoi = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {

				GlobalInstance.selectedInfo = GlobalInstance.listPoi
						.get(msg.arg1).info;

				showRoute();
			}
			super.handleMessage(msg);
		};

	};

	Handler hJump = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {
				setMode(true);
			}
			super.handleMessage(msg);
		};
	};

	Handler hShowPoiInfo = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {
				showTouchedButton(msg.arg1);
				showTouchedInfo(msg.arg1 != -1);
			}
			super.handleMessage(msg);
		};

	};

	// [region] lift circle
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.poi_list);
		init();
		keyword = getIntent().getStringExtra("keyword");
		tvName.setText(String.format(getString(R.string.nearby), keyword));
		GlobalInstance.search.start();

	}

	@Override
	protected void onResume() {

		registerReceiver(myreceiver, mapFilter);
		registerReceiver(searchReceiver, searchFilter);

		GlobalInstance.search.start();
		if (loading) {
			loading = false;
			GlobalInstance.search.searchPoi(keyword);
		}

		if (GlobalInstance.selectedInfo != null) {
			mvMap.getController().setCenter(GlobalInstance.selectedInfo.pt);
			Log.e("Zoom", String.valueOf(mvMap.getZoomLevel()));
		} else if (GlobalInstance.point != null) {
			mvMap.getController().animateTo(GlobalInstance.point);
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

	private void initGlobal() {
		MainApplication app = (MainApplication) getApplication();
		super.initMapActivity(app.getMapManager());

	}

	@Override
	protected void init() {
		super.init();
		initGlobal();
		initMapComp();
		backArea.setOnClickListener(this);
		lvPoi.setOnItemClickListener(this);
		tvLoading.setVisibility(View.VISIBLE);
		btnRight.setOnClickListener(this);
		btnReturnList.setOnClickListener(this);
		btnPoiDistance.setOnClickListener(this);
	}

	private void initMapComp() {

		mvMap.setDrawOverlayWhenZooming(true);
		mvMap.getController().setZoom(mvMap.getMaxZoomLevel() - 2);
		mvMap.setDoubleClickZooming(true);
		mvMap.getController().setCenter(GlobalInstance.point);

		marker = getResources().getDrawable(R.drawable.marker);
		marker.setBounds(0, 0, UIUtils.dipToPx(44), UIUtils.dipToPx(50));
		markerGreen = getResources().getDrawable(R.drawable.marker_focus);
		markerGreen.setBounds(0, 0, UIUtils.dipToPx(44), UIUtils.dipToPx(50));

		noTouchOverlay = new NoTouchOverlay(this, marker, markerGreen, hJump,
				hShowPoiInfo);
		mvMap.getOverlays().add(noTouchOverlay);
	}

	@Override
	protected void mappingComponents() {
		super.mappingComponents();
		mvMap = (MapView) findViewById(R.id.mvMap);
		tvLoading = (TextView) findViewById(R.id.tvLoading);
		lvPoi = (ListView) findViewById(R.id.lvPoi);
		tvName.setText(R.string.list_result);

		layMapBottom = (RelativeLayout) findViewById(R.id.layMapBottom);
		layGeoItem = (RelativeLayout) findViewById(R.id.layGeoItem);
		btnReturnList = (Button) findViewById(R.id.btnReturnList);
		tvPoiName = (TextView) findViewById(R.id.tvPoiName);
		tvPoiAddress = (TextView) findViewById(R.id.tvPoiAddress);
		btnPoiDistance = (Button) findViewById(R.id.btnPoiDistance);
	}

	// [region] events
	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.backArea:
			finish();
			break;
		case R.id.btnRight:
			mvMap.getController().animateTo(GlobalInstance.point);
			break;
		case R.id.btnReturnList:
			setMode(false);
			break;
		case R.id.btnPoiDistance:
			showRoute();
			break;
		}

	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {

		GlobalInstance.selectedInfo = GlobalInstance.listPoi.get(position).info;
		showTouchedButton(position);
		noTouchOverlay.onTap(position);
		mvMap.getController().animateTo(GlobalInstance.selectedInfo.pt);

	}

	private void showTouchedButton(int position) {
		for (int i = 0; i < GlobalInstance.listPoi.size(); i++) {
			GlobalInstance.listPoi.get(i).showButton = (i == position);
		}
		adapter.notifyDataSetChanged();
	}

	private void showTouchedInfo(boolean hasItem) {
		if (!hasItem) {
			btnReturnList.setVisibility(View.VISIBLE);
			layGeoItem.setVisibility(View.GONE);
		} else {
			layGeoItem.setVisibility(View.VISIBLE);
			btnReturnList.setVisibility(View.GONE);
			tvPoiName.setText(GlobalInstance.selectedInfo.name);
			tvPoiAddress.setText(GlobalInstance.selectedInfo.address);
		}
	}

	private void setMode(boolean mapMode) {
		this.mapMode = mapMode;
		if (mapMode) {
			lvPoi.setVisibility(View.GONE);
			layMapBottom.setVisibility(View.VISIBLE);
			RelativeLayout.LayoutParams mapParam = (RelativeLayout.LayoutParams) mvMap
					.getLayoutParams();
			mapParam.height = RelativeLayout.LayoutParams.MATCH_PARENT;
			// mapParam.addRule(RelativeLayout.ABOVE, R.id.layMapBottom);
			mvMap.setLayoutParams(mapParam);
			mvMap.getController().setZoom(mvMap.getMaxZoomLevel());
			if (GlobalInstance.selectedInfo != null) {
				mvMap.getController().setCenter(GlobalInstance.selectedInfo.pt);
				showTouchedInfo(true);
			} else {
				showTouchedInfo(false);
			}
		} else {
			layMapBottom.setVisibility(View.GONE);
			lvPoi.setVisibility(View.VISIBLE);
			RelativeLayout.LayoutParams mapParam = (RelativeLayout.LayoutParams) mvMap
					.getLayoutParams();
			mapParam.height = UIUtils.dipToPx(240);
			// mapParam.addRule(RelativeLayout.ABOVE, 0);
			mvMap.setLayoutParams(mapParam);
			mvMap.getController().setZoom(mvMap.getMaxZoomLevel() - 2);
			mvMap.getController().animateTo(GlobalInstance.point);
		}
		noTouchOverlay.setMapMode(mapMode);

	}

	private void showRoute() {
		Intent inMap = new Intent(this, MapRouteActivity.class);
		inMap.putExtra("style", mapMode ? 1 : 0);
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
						adapter = new PoiAdapter(PoiListActivity.this,
								getLayoutInflater(), GlobalInstance.listPoi,
								hPoi);
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
				adapter = new PoiAdapter(PoiListActivity.this,
						getLayoutInflater(), GlobalInstance.listPoi, hPoi);
				// initMapPos();
				lvPoi.setAdapter(adapter);
			}
			noTouchOverlay.clearAll();
			for (PoiInfoEx info : GlobalInstance.listPoi) {
				noTouchOverlay
						.addOverlay(new OverlayItem(info.info.pt, "", ""));
			}
			noTouchOverlay.update();

			GlobalInstance.search.start();
		}
	}

	private MapReceiver myreceiver = new MapReceiver();
	private IntentFilter mapFilter = new IntentFilter(
			MainApplication.NETWORK_ERROR_ACTION);

	private SearchReceiver searchReceiver = new SearchReceiver();
	private IntentFilter searchFilter = new IntentFilter(
			MainApplication.POI_FOUND_ACTION);
}
