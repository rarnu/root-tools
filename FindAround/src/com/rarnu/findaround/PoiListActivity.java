package com.rarnu.findaround;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;

import com.baidu.mapapi.MapView;
import com.baidu.mapapi.OverlayItem;
import com.baidu.mapapi.MapView.LayoutParams;
import com.rarnu.findaround.adapter.PoiAdapter;
import com.rarnu.findaround.api.BaiduAPI;
import com.rarnu.findaround.base.BaseMapActivity;
import com.rarnu.findaround.common.GeoPointOri;
import com.rarnu.findaround.common.UIUtils;
import com.rarnu.findaround.comp.PopupView;
import com.rarnu.findaround.map.MarkOverlay;
import com.rarnu.findaround.map.SelfPosOverlay;

public class PoiListActivity extends BaseMapActivity implements
		OnClickListener, OnItemClickListener {

	private static final int POPUP_ID = 100001;

	MapView mvMap;
	SelfPosOverlay overlay;
	MarkOverlay markOverlay;
	ListView lvPoi;
	TextView tvLoading;
	PoiAdapter adapter;
	String keyword;
	PopupView popup;

	boolean loading = true;

	Handler hPoi = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {

				GlobalInstance.selectedInfo = GlobalInstance.listPoi
						.get(msg.arg1).info;

				Intent inMap = new Intent(PoiListActivity.this,
						MapRouteActivity.class);
				startActivity(inMap);
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
		GlobalInstance.search.start();

	}

	@Override
	protected void onResume() {

		registerReceiver(myreceiver, mapFilter);
		registerReceiver(searchReceiver, searchFilter);

		overlay.enableMyLocation();
		GlobalInstance.search.start();
		if (loading) {
			loading = false;
			GlobalInstance.search.searchPoi(keyword);
		}

		super.onResume();

	}

	@Override
	protected void onPause() {
		overlay.disableMyLocation();
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
		btnLeft.setOnClickListener(this);
		tvName.setOnClickListener(this);
		lvPoi.setOnItemClickListener(this);
		tvLoading.setVisibility(View.VISIBLE);
		btnRight.setOnClickListener(this);
	}

	private void initMapComp() {

		mvMap.setDrawOverlayWhenZooming(true);
		mvMap.getController().setZoom(mvMap.getMaxZoomLevel() - 2);
		mvMap.setDoubleClickZooming(true);
		mvMap.getController().setCenter(GlobalInstance.point);

		overlay = new SelfPosOverlay(this, mvMap);
		mvMap.getOverlays().add(overlay);

		Drawable marker = getResources().getDrawable(R.drawable.marker);
		marker.setBounds(0, 0, UIUtils.dipToPx(14), UIUtils.dipToPx(18));

		popup = new PopupView(this);
		popup.setId(POPUP_ID);
		mvMap.addView(popup, new MapView.LayoutParams(
				LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT, null,
				MapView.LayoutParams.TOP_LEFT));
		popup.setVisibility(View.GONE);
		popup.setOnClickListener(this);

		markOverlay = new MarkOverlay(marker, popup, mvMap);
		mvMap.getOverlays().add(markOverlay);

		// Drawable marker = getResources().getDrawable(R.drawable.marker);
		// marker.setBounds(0, 0, UIUtils.dipToPx(14), UIUtils.dipToPx(18));

		// popup = new PopupView(this);
		// popup.setId(POPUP_ID);
		// mvMap.addView(popup, new MapView.LayoutParams(
		// LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT, null,
		// MapView.LayoutParams.TOP_LEFT));
		// popup.setVisibility(View.GONE);

		// markOverlay = new MarkOverlay(marker, popup, mvMap);
		// mvMap.getOverlays().add(markOverlay);
		// markOverlay.clearAll();
		// for (int i = 0; i < GlobalInstance.listPoi.size(); i++) {
		// markOverlay.addOverlay(new OverlayItem(GlobalInstance.listPoi
		// .get(i).pt, GlobalInstance.listPoi.get(i).name,
		// GlobalInstance.listPoi.get(i).address));
		// }

	}

	@Override
	protected void mappingComponents() {
		super.mappingComponents();
		mvMap = (MapView) findViewById(R.id.mvMap);
		tvLoading = (TextView) findViewById(R.id.tvLoading);
		lvPoi = (ListView) findViewById(R.id.lvPoi);
		tvName.setText(R.string.list_result);
	}

	// private void initMapPos() {
	//
	// String url = "http://api.map.baidu.com/staticimage?";
	// url += String.format("center=%f,%f&width=%d&height=%d&zoom=%d",
	// GlobalInstance.pointOri.longitude,
	// GlobalInstance.pointOri.latitude,
	// UIUtils.pxToDip(UIUtils.getWidth()), 200, 16);
	//
	// String markers = "";
	// double lat, lng;
	// for (int i = 0; i < GlobalInstance.listPoi.size(); i++) {
	// lat = (double) (((double) GlobalInstance.listPoi.get(i).pt
	// .getLatitudeE6()) / 1e6);
	// lng = (double) (((double) GlobalInstance.listPoi.get(i).pt
	// .getLongitudeE6()) / 1e6);
	// markers += String.format("%f,%f|", lng, lat);
	// }
	// url += "&markers=" + markers;
	// url += "&markerStyles=s,%20,0xff0000";
	//
	// MapHead imgMap = new MapHead(this);
	// imgMap.setLayoutParams(new AbsListView.LayoutParams(
	// LayoutParams.MATCH_PARENT, 0));
	// Log.e("BAIDU", url);
	// NetFiles.doDownloadImageT(this, url, "tmp.jpg", imgMap, lvPoi);
	// lvPoi.addHeaderView(imgMap);
	// }

	// [region] events
	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.tvName:
		case R.id.btnLeft:

			finish();
			break;
		case R.id.btnRight:
			mvMap.getController().animateTo(GlobalInstance.point);
			break;
		case POPUP_ID:
			Intent inMap = new Intent(PoiListActivity.this,
					MapRouteActivity.class);
			startActivity(inMap);
			break;
		}

	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {

		GlobalInstance.selectedInfo = GlobalInstance.listPoi.get(position).info;
		showTouchedButton(position);
		markOverlay.clearAll();

		markOverlay.addOverlay(new OverlayItem(GlobalInstance.selectedInfo.pt,
				GlobalInstance.selectedInfo.name, ""));
		mvMap.getController().animateTo(GlobalInstance.selectedInfo.pt);

		// if (position == 0) {
		// GlobalInstance.selectedInfo = null;
		// } else {
		// GlobalInstance.selectedInfo = GlobalInstance.listPoi
		// .get(position - 1);
		// }
		//
		// Intent inMap = new Intent(this, MapRouteActivity.class);
		// startActivity(inMap);

	}

	private void showTouchedButton(int position) {
		for (int i = 0; i < GlobalInstance.listPoi.size(); i++) {
			GlobalInstance.listPoi.get(i).showButton = (i == position);
		}
		adapter.notifyDataSetChanged();
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
