package com.rarnu.findaround;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.baidu.mapapi.MapView;
import com.baidu.mapapi.Overlay;
import com.baidu.mapapi.RouteOverlay;
import com.rarnu.findaround.base.BaseMapActivity;
import com.rarnu.findaround.common.Config;
import com.rarnu.findaround.common.UIUtils;
import com.rarnu.findaround.map.SelfPosOverlay;

public class MapRouteActivity extends BaseMapActivity implements
		OnClickListener {

	// private static final int ZOOM_MAX = 18;
	// private static final int ZOOM_MIN = 3;
	// private static final int POPUP_ID = 100001;

	// [region] map
	MapView mvMap;

	SelfPosOverlay overlay;
	// MarkOverlay markOverlay;
	String city;

	RouteOverlay walkOverlay;
	RouteOverlay driveOverlay;

	Drawable markerStart, markerEnd;
	int listStyle = 0;
	// PopupView popup;

	// [/region]

	// [region] field define

	TextView tvLoading;
	RelativeLayout layRoute;
	RelativeLayout btnPrior, btnNext;
	ImageView imgPrior, imgNext;
	TextView tvRoute;

	// [/region]

	// [region] life circle
	@Override
	protected void onCreate(Bundle arg0) {
		super.onCreate(arg0);
		setContentView(R.layout.main);
		listStyle = getIntent().getIntExtra("style", 0);
		init();
	}

	@Override
	protected void onResume() {

		overlay.enableCompass();
		overlay.enableMyLocation();

		GlobalInstance.search.locate();

		if (GlobalInstance.selectedInfo != null) {
			tvLoading.setVisibility(View.VISIBLE);
			if (Config.getMethod(this) == 2) {
				GlobalInstance.search.searchWalk(GlobalInstance.point,
						GlobalInstance.selectedInfo.pt);
			} else {
				GlobalInstance.search.searchDrive(GlobalInstance.point,
						GlobalInstance.selectedInfo.pt);
			}
		} else {
			tvLoading.setVisibility(View.GONE);
		}

		super.onResume();
		registerReceiver(myreceiver, mapFilter);
	}

	@Override
	protected void onPause() {

		overlay.disableCompass();
		overlay.disableMyLocation();
		GlobalInstance.search.stop();
		unregisterReceiver(myreceiver);
		super.onPause();
	}

	@Override
	protected void onDestroy() {
		mvMap.getController().setZoom(
				listStyle == 0 ? mvMap.getMaxZoomLevel() - 2 : mvMap
						.getMaxZoomLevel());
		super.onDestroy();
	}

	// [/region]

	// [region] init

	@Override
	protected void init() {
		super.init();
		initGlobal();
		initMapComp();
		initEvents();
		if (GlobalInstance.selectedInfo == null) {
			tvName.setText(R.string.view_map);
		} else {
			tvName.setText(GlobalInstance.selectedInfo.name);
		}

	}

	private void initGlobal() {
		MainApplication app = (MainApplication) getApplication();
		super.initMapActivity(app.getMapManager());

	}

	@Override
	protected void mappingComponents() {
		super.mappingComponents();
		tvLoading = (TextView) findViewById(R.id.tvLoading);
		mvMap = (MapView) findViewById(R.id.mvMap);

		layRoute = (RelativeLayout) findViewById(R.id.layRoute);
		btnPrior = (RelativeLayout) findViewById(R.id.btnPrior);
		btnNext = (RelativeLayout) findViewById(R.id.btnNext);
		imgPrior = (ImageView) findViewById(R.id.imgPrior);
		imgNext = (ImageView) findViewById(R.id.imgNext);
		tvRoute = (TextView) findViewById(R.id.tvRoute);

		setRouteButtonVisible(false);
	}

	private void initMapComp() {

		mvMap.getController().setCenter(GlobalInstance.point);

		mvMap.setDrawOverlayWhenZooming(true);
		mvMap.getController().setZoom(mvMap.getMaxZoomLevel());
		mvMap.setDoubleClickZooming(true);

		overlay = new SelfPosOverlay(this, mvMap, true);
		mvMap.getOverlays().add(overlay);

		// popup = new PopupView(this);
		// popup.setId(POPUP_ID);
		// mvMap.addView(popup, new MapView.LayoutParams(
		// LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT, null,
		// MapView.LayoutParams.TOP_LEFT));
		// popup.setVisibility(View.GONE);
		// popup.setOnClickListener(this);

		markerStart = getResources().getDrawable(R.drawable.mypos);
		markerStart.setBounds(0, 0, UIUtils.dipToPx(14), UIUtils.dipToPx(22));

		markerEnd = getResources().getDrawable(R.drawable.marker_focus);
		markerEnd.setBounds(0, 0, UIUtils.dipToPx(19), UIUtils.dipToPx(24));

		// markOverlay = new MarkOverlay(markerEnd, popup, mvMap);
		// mvMap.getOverlays().add(markOverlay);

	}

	private void initEvents() {

		backArea.setOnClickListener(this);
		btnRight.setOnClickListener(this);
		btnPrior.setOnClickListener(this);
		btnNext.setOnClickListener(this);

	}

	// [/region]

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
		case R.id.btnPrior:
			setRouteShow(false);
			break;

		case R.id.btnNext:
			setRouteShow(true);
			break;

		}

	}

	// [/region]

	// [region] map callbacks

	public void showRoute(boolean walk) {
		tvLoading.setVisibility(View.GONE);
		setRouteButtonVisible(false);
		removeRouteOverlays();

		if (walk) {
			walkOverlay = new RouteOverlay(this, mvMap);
			walkOverlay.setData(GlobalInstance.selectedRoute);
			walkOverlay.getItem(0).setMarker(markerStart);
			walkOverlay.getItem(GlobalInstance.selectedRoute.getNumSteps() - 1)
					.setMarker(markerEnd);
			mvMap.getOverlays().add(walkOverlay);
		} else {
			driveOverlay = new RouteOverlay(this, mvMap);
			driveOverlay.setData(GlobalInstance.selectedRoute);
			driveOverlay.getItem(0).setMarker(markerStart);
			driveOverlay
					.getItem(GlobalInstance.selectedRoute.getNumSteps() - 1)
					.setMarker(markerEnd);
			mvMap.getOverlays().add(driveOverlay);
		}

		// markOverlay.clearAll();
		// markOverlay.addOverlay(new
		// OverlayItem(GlobalInstance.selectedInfo.pt,
		// GlobalInstance.selectedInfo.name,
		// GlobalInstance.selectedInfo.address));

		mvMap.invalidate();
		mvMap.getController()
				.animateTo(GlobalInstance.selectedRoute.getStart());
		setRouteButtonVisible(true);
		setRouteShow(true);

	}

	@Override
	protected boolean isRouteDisplayed() {
		return false;
	}

	// [/region]

	// [region] common functions

	private void removeRouteOverlays() {
		removeRouteOverlay(walkOverlay);
		removeRouteOverlay(driveOverlay);
		mvMap.invalidate();
	}

	private void removeRouteOverlay(Overlay o) {
		try {
			mvMap.getOverlays().remove(o);
		} catch (Exception e) {

		}
	}

	// private boolean isGpuAccelerateEnabled() {
	// boolean ret = false;
	//
	// try {
	// if (android.os.Build.VERSION.SDK_INT >= 14) {
	// ret = SystemProperties.getBoolean(HARDWARE_UI_PROPERTY, false);
	// }
	// } catch (Exception e) {
	// Toast.makeText(
	// this,
	// "Not standard Android OS, please check GPU settings manually.",
	// Toast.LENGTH_LONG).show();
	// ret = false;
	// }
	// return ret;
	// }

	private void setRouteButtonVisible(boolean visible) {
		layRoute.setVisibility(visible ? View.VISIBLE : View.GONE);
	}

	private void setRouteShow(boolean next) {
		if (next) {
			if (GlobalInstance.routeIndex >= (GlobalInstance.selectedRoute
					.getNumSteps() - 2)) {
				return;
			}
			GlobalInstance.routeIndex++;
		} else {
			if (GlobalInstance.routeIndex <= 0) {
				return;
			}
			GlobalInstance.routeIndex--;
		}

		if (GlobalInstance.routeIndex == GlobalInstance.selectedRoute
				.getNumSteps() - 2) {
			tvRoute.setText(getString(R.string.terminal)
					+ GlobalInstance.selectedInfo.address);
		} else {
			tvRoute.setText(GlobalInstance.selectedRoute.getStep(
					GlobalInstance.routeIndex).getContent());
		}
		mvMap.getController().animateTo(
				GlobalInstance.selectedRoute.getStep(GlobalInstance.routeIndex)
						.getPoint());
		imgPrior.setBackgroundResource(GlobalInstance.routeIndex <= 0 ? R.drawable.route_left_disabled
				: R.drawable.route_left);
		imgNext.setBackgroundResource(GlobalInstance.routeIndex >= (GlobalInstance.selectedRoute
				.getNumSteps() - 2) ? R.drawable.route_right_disabled
				: R.drawable.route_right);

	}

	// [/region]

	// [region] receiver
	class MapReceiver extends BroadcastReceiver {

		@Override
		public void onReceive(Context context, Intent intent) {
			showRoute(Config.getMethod(context) == 2);
		}

	}

	private MapReceiver myreceiver = new MapReceiver();
	private IntentFilter mapFilter = new IntentFilter(
			MainApplication.ROUTE_FOUND_ACTION);
	// [/region]
}
