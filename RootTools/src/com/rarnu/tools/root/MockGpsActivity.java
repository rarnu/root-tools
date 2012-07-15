package com.rarnu.tools.root;

import java.util.List;

import android.location.Location;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.TextView;
import android.widget.Toast;

import com.baidu.mapapi.GeoPoint;
import com.baidu.mapapi.LocationListener;
import com.baidu.mapapi.MKAddrInfo;
import com.baidu.mapapi.MKBusLineResult;
import com.baidu.mapapi.MKDrivingRouteResult;
import com.baidu.mapapi.MKLocationManager;
import com.baidu.mapapi.MKPoiInfo;
import com.baidu.mapapi.MKPoiResult;
import com.baidu.mapapi.MKSearch;
import com.baidu.mapapi.MKSearchListener;
import com.baidu.mapapi.MKSuggestionResult;
import com.baidu.mapapi.MKTransitRouteResult;
import com.baidu.mapapi.MKWalkingRouteResult;
import com.baidu.mapapi.MapActivity;
import com.baidu.mapapi.MapView;
import com.baidu.mapapi.OverlayItem;
import com.rarnu.tools.root.base.ActivityIntf;
import com.rarnu.tools.root.comp.MarkOverlay;
import com.rarnu.tools.root.comp.MockBar;
import com.rarnu.tools.root.comp.SearchBar;
import com.rarnu.tools.root.comp.TitleBar;
import com.rarnu.tools.root.service.MockLocationService;

public class MockGpsActivity extends MapActivity implements ActivityIntf,
		OnClickListener, LocationListener, MKSearchListener {

	// [region] field define
	TitleBar tbTitle;
	SearchBar sbMock;
	MapView mvMock;
	MockBar mbMock;
	MKSearch mkSearch;
	MarkOverlay overlay;
	Button btnMockPrior, btnMockNext;
	TextView tvMockPage;

	// [/region]

	// [region] variable
	boolean getCity = true;

	List<MKPoiInfo> listPoi = null;
	int currentPoi = -1;

	// [/region]

	// [region] life circle
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.layout_mockgps);
		init();
	}

	@Override
	protected void onResume() {
		RootApplication app = (RootApplication) getApplication();
		app.getMapManager().getLocationManager().requestLocationUpdates(this);
		app.getMapManager().getLocationManager()
				.enableProvider(MKLocationManager.MK_GPS_PROVIDER);
		app.getMapManager().getLocationManager()
				.enableProvider(MKLocationManager.MK_NETWORK_PROVIDER);
		app.getMapManager().start();
		super.onResume();
	}

	@Override
	protected void onPause() {
		RootApplication app = (RootApplication) getApplication();
		app.getMapManager().getLocationManager().removeUpdates(this);
		app.getMapManager().getLocationManager()
				.disableProvider(MKLocationManager.MK_GPS_PROVIDER);
		app.getMapManager().getLocationManager()
				.disableProvider(MKLocationManager.MK_NETWORK_PROVIDER);
		app.getMapManager().stop();
		super.onPause();
	}

	// [/region]

	// [region] init
	@Override
	public void init() {
		RootApplication app = (RootApplication) getApplication();
		app.getMapManager().start();
		super.initMapActivity(app.getMapManager());

		mappingComp();
		initTitle();
		initSearchBar();
		initEvents();

		mvMock.setDrawOverlayWhenZooming(true);
		mvMock.getController().setZoom(mvMock.getMaxZoomLevel());

		mkSearch = new MKSearch();
		mkSearch.init(app.getMapManager(), this);

		overlay = new MarkOverlay(getResources().getDrawable(
				R.drawable.iconmarka), mvMock);
		mvMock.getOverlays().add(overlay);

		btnMockPrior.setEnabled(false);
		btnMockNext.setEnabled(false);
		refreshPoiPage();
	}

	@Override
	public void mappingComp() {
		tbTitle = (TitleBar) findViewById(R.id.tbTitle);
		sbMock = (SearchBar) findViewById(R.id.sbMock);
		mvMock = (MapView) findViewById(R.id.mvMock);
		mbMock = (MockBar) findViewById(R.id.mbMock);
		btnMockPrior = (Button) findViewById(R.id.btnMockPrior);
		btnMockNext = (Button) findViewById(R.id.btnMockNext);
		tvMockPage = (TextView) findViewById(R.id.tvMockPage);
	}

	@Override
	public void initTitle() {
		tbTitle.setText(getString(R.string.func8_title));
		tbTitle.setLeftButtonText(getString(R.string.back));
		tbTitle.getLeftButton().setVisibility(View.VISIBLE);
		tbTitle.getRightButton().setText(getString(R.string.city));
		tbTitle.getRightButton().setVisibility(View.VISIBLE);
	}

	@Override
	public void initSearchBar() {
		sbMock.setAddButtonVisible(false);
		sbMock.getCancelButton().setBackgroundResource(R.drawable.remote);
		sbMock.getEditText().setHint(R.string.input_hint_mock);
	}

	@Override
	public void initEvents() {
		tbTitle.getLeftButton().setOnClickListener(this);
		tbTitle.getRightButton().setOnClickListener(this);
		sbMock.getCancelButton().setOnClickListener(this);
		mbMock.getMockButton().setOnClickListener(this);
		mbMock.getMockHistoryButton().setOnClickListener(this);
		btnMockPrior.setOnClickListener(this);
		btnMockNext.setOnClickListener(this);
	}

	// [/region]

	// [region] events
	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnLeft:
			finish();
			break;
		case R.id.btnRight:
			// TODO: switch city
			break;
		case R.id.btnCancel:
			if (tbTitle.getRightButton().getText().toString()
					.equals(getString(R.string.city))
					|| tbTitle.getRightButton().getText().toString().equals("")) {
				Toast.makeText(this, R.string.no_city_found, Toast.LENGTH_LONG)
						.show();
				return;
			}
			mkSearch.poiSearchInCity(tbTitle.getRightButton().getText()
					.toString(), sbMock.getText().toString());
			break;
		case R.id.btnMock:
			mock();
			break;
		case R.id.btnMockHistory:
			// TODO: show mock history
			break;
		case R.id.btnMockPrior:
			if (listPoi != null && listPoi.size() != 0) {
				if (currentPoi > 1) {
					currentPoi--;
					mvMock.getController().animateTo(
							listPoi.get(currentPoi - 1).pt);
					refreshPoiPage();
				}
			}
			break;
		case R.id.btnMockNext:
			if (listPoi != null && listPoi.size() != 0) {
				if (currentPoi < listPoi.size()) {
					currentPoi++;
					mvMock.getController().animateTo(
							listPoi.get(currentPoi - 1).pt);
					refreshPoiPage();
				}
			}
			break;
		}
	}

	@Override
	protected boolean isRouteDisplayed() {
		return false;
	}

	@Override
	public void onLocationChanged(Location loc) {
		GeoPoint geo = new GeoPoint((int) (loc.getLatitude() * 1e6),
				(int) (loc.getLongitude() * 1e6));

		mvMock.getController().setCenter(geo);
		mkSearch.reverseGeocode(geo);

	}

	@Override
	public void onGetAddrResult(MKAddrInfo res, int arg1) {
		if (res != null) {
			if (getCity) {
				getCity = false;
				tbTitle.getRightButton().setText(res.addressComponents.city);
				((RootApplication) getApplication()).getMapManager()
						.getLocationManager().removeUpdates(this);
			} else {
				mbMock.setMockAddress(res.strAddr);
			}
		}
	}

	@Override
	public void onGetBusDetailResult(MKBusLineResult res, int arg1) {

	}

	@Override
	public void onGetDrivingRouteResult(MKDrivingRouteResult res, int arg1) {

	}

	@Override
	public void onGetPoiResult(MKPoiResult res, int arg1, int arg2) {
		overlay.clearAll();
		listPoi = null;
		currentPoi = 0;
		refreshPoiPage();
		btnMockNext.setEnabled(false);
		btnMockPrior.setEnabled(false);
		if (res == null || res.getAllPoi() == null
				|| res.getAllPoi().size() == 0) {
			Toast.makeText(this, R.string.no_suggestion_found,
					Toast.LENGTH_LONG).show();
			return;
		}
		listPoi = res.getAllPoi();
		for (MKPoiInfo info : listPoi) {
			overlay.addOverlay(new OverlayItem(info.pt, "", ""));
		}
		mvMock.getController().animateTo(listPoi.get(0).pt);
		currentPoi = 1;
		refreshPoiPage();
		mvMock.invalidate();
		btnMockNext.setEnabled(true);
		btnMockPrior.setEnabled(true);
	}

	@Override
	public void onGetSuggestionResult(MKSuggestionResult res, int arg1) {

	}

	@Override
	public void onGetTransitRouteResult(MKTransitRouteResult res, int arg1) {

	}

	@Override
	public void onGetWalkingRouteResult(MKWalkingRouteResult res, int arg1) {

	}

	// [/region]

	// [region] business logic
	private void mock() {
		if (MockLocationService._instance == null) {
			doMock();
			mbMock.getMockButton().setText(R.string.stop);
		} else {
			doUnmock();
			mbMock.getMockButton().setText(R.string.mock);
		}
	}

	private void doMock() {
		// TODO: mock
		mbMock.setMockGeo(mvMock.getMapCenter());
		mkSearch.reverseGeocode(mvMock.getMapCenter());
	}

	private void doUnmock() {
		// TODO: unmock
	}

	private void refreshPoiPage() {
		if (listPoi == null) {
			tvMockPage.setText("0/0");
		} else {
			tvMockPage.setText(String.format("%d/%d", currentPoi,
					listPoi.size()));
		}
	}
	// [/region]
}
