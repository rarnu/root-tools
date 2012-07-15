package com.rarnu.findaround;

import java.util.List;

import android.app.AlertDialog;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.IntentFilter;
import android.graphics.Color;
import android.graphics.drawable.Drawable;
import android.location.Location;
import android.os.Bundle;
import android.os.SystemProperties;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.HorizontalScrollView;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.Toast;

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
import com.baidu.mapapi.MapActivity;
import com.baidu.mapapi.MapView;
import com.baidu.mapapi.MapView.LayoutParams;
import com.baidu.mapapi.Overlay;
import com.baidu.mapapi.OverlayItem;
import com.baidu.mapapi.RouteOverlay;

public class MainActivity extends MapActivity implements LocationListener,
		OnClickListener, MKSearchListener {

	// private static final int ZOOM_MAX = 18;
	// private static final int ZOOM_MIN = 3;
	private static final int POPUP_ID = 100001;
	private static final String HARDWARE_UI_PROPERTY = "persist.sys.ui.hw";

	// [region] map
	MapView mvMap;
	MKSearch mSearch;
	SelfPosOverlay overlay;
	MarkOverlay markOverlay;
	GeoPoint point = null;
	String city;
	String address;
	PopupView popup;

	RouteOverlay walkOverlay;
	RouteOverlay driveOverlay;

	// [/region]

	// [region] field define
	Button btnRight;
	HorizontalScrollView hFunc;
	TextView tvLoading;
	Button btnLocate, btnAddress, btnSettings;
	RelativeLayout layFunc;

	// [/region]

	// [region] life circle
	@Override
	protected void onCreate(Bundle arg0) {
		super.onCreate(arg0);
		UIUtils.initDisplayMetrics(getWindowManager());
		setContentView(R.layout.main);

		if (isGpuAccelerateEnabled()) {
			new AlertDialog.Builder(this)
					.setTitle(R.string.hint)
					.setMessage(R.string.gpu_40)
					.setPositiveButton(R.string.ok,
							new DialogInterface.OnClickListener() {

								@Override
								public void onClick(DialogInterface dialog,
										int which) {
									finish();

									Intent inDO = new Intent(
											"android.settings.APPLICATION_DEVELOPMENT_SETTINGS");
									inDO.addCategory("android.intent.category.DEFAULT");
									startActivity(inDO);

								}
							}).show();
		}

		initGlobal();
		mappingComponents();
		initMapComp();
		initEvents();
		initPlaceButtons();
		setFuncBarVisible(false);

	}

	@Override
	protected void onResume() {
		if (point == null) {
			tvLoading.setText(R.string.locating);
			tvLoading.setVisibility(View.VISIBLE);
		}
		MainApplication app = (MainApplication) getApplication();
		app.getMapManager().getLocationManager()
				.enableProvider(MKLocationManager.MK_NETWORK_PROVIDER);
		app.getMapManager().getLocationManager()
				.enableProvider(MKLocationManager.MK_GPS_PROVIDER);
		app.getMapManager().getLocationManager().requestLocationUpdates(this);
		overlay.enableCompass();
		overlay.enableMyLocation();
		app.getMapManager().start();
		super.onResume();
		registerReceiver(myreceiver, mapFilter);
	}

	@Override
	protected void onPause() {
		MainApplication app = (MainApplication) getApplication();
		app.getMapManager().getLocationManager().removeUpdates(this);
		app.getMapManager().getLocationManager()
				.disableProvider(MKLocationManager.MK_NETWORK_PROVIDER);
		app.getMapManager().getLocationManager()
				.disableProvider(MKLocationManager.MK_GPS_PROVIDER);
		overlay.disableCompass();
		overlay.disableMyLocation();
		app.getMapManager().stop();
		unregisterReceiver(myreceiver);
		super.onPause();
	}

	// [/region]

	// [region] init
	private void initGlobal() {
		MainApplication app = (MainApplication) getApplication();
		app.getMapManager().start();

		super.initMapActivity(app.getMapManager());

		mSearch = new MKSearch();
		mSearch.init(app.getMapManager(), this);

	}

	private void mappingComponents() {
		tvLoading = (TextView) findViewById(R.id.tvLoading);
		mvMap = (MapView) findViewById(R.id.mvMap);
		btnRight = (Button) findViewById(R.id.btnRight);

		btnRight.setVisibility(View.VISIBLE);
		btnRight.setText(R.string.search);

		hFunc = (HorizontalScrollView) findViewById(R.id.hFunc);
		layFunc = (RelativeLayout) findViewById(R.id.layFunc);
		btnLocate = (Button) findViewById(R.id.btnLocate);
		btnAddress = (Button) findViewById(R.id.btnAddress);
		btnSettings = (Button) findViewById(R.id.btnSettings);
	}

	private void initMapComp() {
		// locate shanghai first
		mvMap.getController().setCenter(new GeoPoint(31242729, 121510561));

		mvMap.setDrawOverlayWhenZooming(true);
		// mvMap.setBuiltInZoomControls(true);
		mvMap.getController().setZoom(mvMap.getMaxZoomLevel());
		mvMap.setDoubleClickZooming(true);

		overlay = new SelfPosOverlay(this, mvMap);
		mvMap.getOverlays().add(overlay);

		Drawable marker = getResources().getDrawable(R.drawable.iconmarka);
		marker.setBounds(0, 0, marker.getIntrinsicWidth(),
				marker.getIntrinsicHeight());

		popup = new PopupView(this);
		popup.setId(POPUP_ID);
		mvMap.addView(popup, new MapView.LayoutParams(
				LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT, null,
				MapView.LayoutParams.TOP_LEFT));
		popup.setVisibility(View.GONE);

		markOverlay = new MarkOverlay(marker, popup, mvMap);
		mvMap.getOverlays().add(markOverlay);
	}

	private void initEvents() {
		btnRight.setOnClickListener(this);
		btnLocate.setOnClickListener(this);
		btnAddress.setOnClickListener(this);
		btnSettings.setOnClickListener(this);

		popup.setOnClickListener(this);

		// @SuppressWarnings("deprecation")
		// ZoomControls zoom = (ZoomControls) mvMap.getZoomControls();
		// zoom.setOnZoomInClickListener(new OnClickListener() {
		//
		// @Override
		// public void onClick(View v) {
		// int lvl = mvMap.getZoomLevel();
		// if (lvl < ZOOM_MAX) {
		// lvl++;
		// }
		// mvMap.getController().setZoom(lvl);
		// Toast.makeText(MainActivity.this,
		// String.format("Zoom Level: %d", mvMap.getZoomLevel()),
		// Toast.LENGTH_SHORT).show();
		//
		// }
		// });
		//
		// zoom.setOnZoomOutClickListener(new OnClickListener() {
		//
		// @Override
		// public void onClick(View v) {
		// int lvl = mvMap.getZoomLevel();
		// if (lvl > ZOOM_MIN) {
		// lvl--;
		// }
		//
		// mvMap.getController().setZoom(lvl);
		// Toast.makeText(MainActivity.this,
		// String.format("Zoom Level: %d", mvMap.getZoomLevel()),
		// Toast.LENGTH_SHORT).show();
		//
		// }
		// });
	}

	private void initPlaceButtons() {
		removeAllFuncButtons();
		int BID = 1025;
		List<String> list = Config.getKeywordsList(this);
		if (list != null && list.size() != 0) {
			for (int i = 0; i < list.size(); i++) {
				Button btn = new Button(this);
				btn.setId(BID + i);
				btn.setBackgroundResource(R.drawable.button_style);
				btn.setTextColor(Color.WHITE);
				RelativeLayout.LayoutParams rlp = new RelativeLayout.LayoutParams(
						UIUtils.dipToPx(75), UIUtils.dipToPx(40));
				rlp.leftMargin = UIUtils.dipToPx(4);
				rlp.addRule(RelativeLayout.CENTER_VERTICAL, 1);
				if (i == 0) {
					rlp.addRule(RelativeLayout.RIGHT_OF, R.id.btnAddress);
				} else {
					rlp.addRule(RelativeLayout.RIGHT_OF, BID + i - 1);
				}
				btn.setLayoutParams(rlp);
				btn.setText(list.get(i));
				btn.setOnClickListener(new OnClickListener() {

					@Override
					public void onClick(View v) {
						tvLoading.setText(R.string.searching);
						tvLoading.setVisibility(View.VISIBLE);
						mSearch.poiSearchNearBy(((Button) v).getText()
								.toString(), mvMap.getMapCenter(), Config
								.getDist(MainActivity.this));

					}
				});
				layFunc.addView(btn);
			}
			RelativeLayout.LayoutParams rlp = (RelativeLayout.LayoutParams) btnSettings
					.getLayoutParams();
			rlp.addRule(RelativeLayout.RIGHT_OF, BID + list.size() - 1);
			btnSettings.setLayoutParams(rlp);
		}

	}

	private void removeAllFuncButtons() {
		for (int i = layFunc.getChildCount() - 1; i >= 0; i--) {
			if (layFunc.getChildAt(i) instanceof Button) {
				if (layFunc.getChildAt(i).getId() != R.id.btnLocate
						&& layFunc.getChildAt(i).getId() != R.id.btnAddress
						&& layFunc.getChildAt(i).getId() != R.id.btnSettings) {
					layFunc.removeViewAt(i);
				}
			}
		}
		RelativeLayout.LayoutParams rlp = (RelativeLayout.LayoutParams) btnSettings
				.getLayoutParams();
		rlp.addRule(RelativeLayout.RIGHT_OF, R.id.btnAddress);
		btnSettings.setLayoutParams(rlp);
	}

	// [/region]

	// [region] events
	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnRight:
			setFuncBarVisible(!getFuncBarVisible());

			break;
		case R.id.btnLocate:

			if (point != null) {
				mvMap.getController().animateTo(point);
			}
			break;
		case R.id.btnAddress:

			Toast.makeText(this, address, Toast.LENGTH_LONG).show();
			break;

		case R.id.btnSettings:
			Intent inSettings = new Intent(this, SettingsActivity.class);
			startActivityForResult(inSettings, 0);
			break;

		case POPUP_ID:

			if (point == null) {
				Toast.makeText(this, R.string.no_location_found,
						Toast.LENGTH_LONG).show();
				return;
			}
			popup.setVisibility(View.GONE);
			tvLoading.setText(R.string.counting);
			tvLoading.setVisibility(View.VISIBLE);
			MKPlanNode nodeStart = new MKPlanNode();
			nodeStart.pt = point;
			MKPlanNode nodeEnd = new MKPlanNode();
			nodeEnd.pt = markOverlay.getClickedPoint();
			if (Config.getMethod(this) == 2) {
				mSearch.walkingSearch(city, nodeStart, city, nodeEnd);
			} else {
				mSearch.drivingSearch(city, nodeStart, city, nodeEnd);
			}

			break;
		}

	}

	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent data) {
		if (requestCode == 0) {
			initPlaceButtons();
		}
	}

	// [/region]

	// [region] map callbacks
	@Override
	protected boolean isRouteDisplayed() {
		return false;
	}

	@Override
	public void onLocationChanged(Location location) {
		tvLoading.setVisibility(View.GONE);
		if (location == null) {
			point = null;
		} else {
			point = new GeoPoint((int) (location.getLatitude() * 1e6),
					(int) (location.getLongitude() * 1e6));
			mSearch.reverseGeocode(point);
			mvMap.getController().animateTo(point);
		}

	}

	@Override
	public void onGetAddrResult(MKAddrInfo info, int error) {
		if (info != null) {
			address = info.strAddr;
		}
	}

	@Override
	public void onGetBusDetailResult(MKBusLineResult res, int error) {

	}

	@Override
	public void onGetDrivingRouteResult(MKDrivingRouteResult res, int error) {
		tvLoading.setVisibility(View.GONE);
		removeRouteOverlays();
		if (error != 0 || res == null) {
			Toast.makeText(this, R.string.no_result, Toast.LENGTH_SHORT).show();
			return;
		}
		driveOverlay = new RouteOverlay(this, mvMap);
		driveOverlay.setData(res.getPlan(0).getRoute(0));

		mvMap.getOverlays().add(driveOverlay);
		mvMap.invalidate();
		mvMap.getController().animateTo(res.getStart().pt);
	}

	@Override
	public void onGetPoiResult(MKPoiResult res, int type, int error) {
		tvLoading.setVisibility(View.GONE);
		removeRouteOverlays();
		markOverlay.clearAll();

		if (error != 0 || res == null) {
			Toast.makeText(this, R.string.no_result, Toast.LENGTH_LONG).show();
			return;
		}
		if (res.getAllPoi() != null && res.getAllPoi().size() != 0) {
			city = res.getAllPoi().get(0).city;
			for (MKPoiInfo pr : res.getAllPoi()) {

				markOverlay.addOverlay(new OverlayItem(pr.pt, pr.name,
						pr.address));
			}
		}

		mvMap.invalidate();

	}

	@Override
	public void onGetTransitRouteResult(MKTransitRouteResult res, int error) {

	}

	@Override
	public void onGetWalkingRouteResult(MKWalkingRouteResult res, int error) {
		tvLoading.setVisibility(View.GONE);
		removeRouteOverlays();
		if (error != 0 || res == null) {
			Toast.makeText(this, R.string.no_result, Toast.LENGTH_SHORT).show();
			return;
		}
		walkOverlay = new RouteOverlay(this, mvMap);
		walkOverlay.setData(res.getPlan(0).getRoute(0));

		mvMap.getOverlays().add(walkOverlay);
		mvMap.invalidate();

		mvMap.getController().animateTo(res.getStart().pt);

	}

	@Override
	public void onGetSuggestionResult(MKSuggestionResult arg0, int arg1) {

	}

	// [/region]

	// [region] common functions

	private boolean getFuncBarVisible() {
		return getViewVisible(hFunc);
	}

	private void setFuncBarVisible(boolean visible) {
		setViewVisible(hFunc, visible);
	}

	private boolean getViewVisible(View v) {
		return v.getVisibility() == View.VISIBLE;
	}

	private void setViewVisible(View v, boolean visible) {
		RelativeLayout.LayoutParams rlp = (RelativeLayout.LayoutParams) v
				.getLayoutParams();
		rlp.height = (visible ? RelativeLayout.LayoutParams.WRAP_CONTENT : 0);
		v.setLayoutParams(rlp);
		v.setVisibility(visible ? View.VISIBLE : View.INVISIBLE);
	}

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

	private boolean isGpuAccelerateEnabled() {
		boolean ret = false;

		try {
			if (android.os.Build.VERSION.SDK_INT >= 14) {
				ret = SystemProperties.getBoolean(HARDWARE_UI_PROPERTY, false);
			}
		} catch (Exception e) {
			Toast.makeText(
					this,
					"Not standard Android OS, please check GPU settings manually.",
					Toast.LENGTH_LONG).show();
			ret = false;
		}
		return ret;
	}
	// [/region]

	// [region] receiver
	class MapReceiver extends BroadcastReceiver {

		@Override
		public void onReceive(Context context, Intent intent) {
			tvLoading.setVisibility(View.GONE);
			Toast.makeText(context, R.string.network_error, Toast.LENGTH_LONG).show();
		}
		
	}
	
	private MapReceiver myreceiver = new MapReceiver();
	private IntentFilter mapFilter = new IntentFilter(MainApplication.NETWORK_ERROR_ACTION);
	// [/region]
}
