package com.rarnu.findaround;

import java.util.List;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.DialogInterface;
import android.content.DialogInterface.OnCancelListener;
import android.content.Intent;
import android.content.IntentFilter;
import android.location.Location;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.KeyEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.OnLongClickListener;
import android.view.inputmethod.InputMethodManager;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.RelativeLayout.LayoutParams;
import android.widget.TextView;
import android.widget.Toast;

import com.baidu.mapapi.GeoPoint;
import com.baidu.mapapi.LocationListener;
import com.baidu.mapapi.MKLocationManager;
import com.rarnu.findaround.api.BaiduAPI;
import com.rarnu.findaround.base.BaseActivity;
import com.rarnu.findaround.common.CellInfoManager;
import com.rarnu.findaround.common.CellLocationManager;
import com.rarnu.findaround.common.Config;
import com.rarnu.findaround.common.GeoPointOri;
import com.rarnu.findaround.common.PageItem;
import com.rarnu.findaround.common.PageUtils;
import com.rarnu.findaround.common.UIUtils;
import com.rarnu.findaround.common.WifiInfoManager;
import com.rarnu.findaround.comp.AlertDialogEx;
import com.rarnu.findaround.comp.AlertDialogEx.DialogButtonClickListener;
import com.rarnu.findaround.comp.GridPage4x4;
import com.rarnu.findaround.comp.GridPage4x4.OnDeleteClickListener;
import com.rarnu.findaround.comp.GridPage4x4.OnKeywordClickListener;
import com.rarnu.findaround.comp.GridPageSearch;
import com.rarnu.findaround.comp.PointBar;
import com.rarnu.findaround.comp.PopupMenuDialog;
import com.rarnu.findaround.comp.ScrollLayout;
import com.rarnu.findaround.comp.ScrollLayout.OnScreenChangeListener;

public class WelcomeActivity extends BaseActivity implements OnClickListener,
		LocationListener, OnLongClickListener, OnDeleteClickListener,
		OnKeywordClickListener, OnScreenChangeListener {

	ScrollLayout gButtons;
	TextView tvAddress;
	PointBar layPoints;
	PopupMenuDialog menu;
	ImageView ivArr, ivSplit;
	InputMethodManager inputMgr;

	// CellLocationManager locationManager = null;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		UIUtils.initDisplayMetrics(getWindowManager());
		GlobalInstance.pm = getPackageManager();
		inputMgr = (InputMethodManager) getSystemService(Context.INPUT_METHOD_SERVICE);
		setContentView(R.layout.welcome);

		init();

		MainApplication app = (MainApplication) getApplication();
		app.getMapManager().start();

		// if (!SIMUtils.isSimCardReady(this)) {
		// doCellLocation();
		// }

	}

	protected void refresh() {
		setContentView(R.layout.welcome);

		init();
		tvAddress.setText(GlobalInstance.address);

	}

	@Override
	protected void onDestroy() {
		GlobalInstance.point = null;
		super.onDestroy();
	}

	@Override
	protected void onResume() {

		MainApplication app = (MainApplication) getApplication();
		app.getMapManager().getLocationManager()
				.enableProvider(MKLocationManager.MK_NETWORK_PROVIDER);
		app.getMapManager().getLocationManager()
				.enableProvider(MKLocationManager.MK_GPS_PROVIDER);
		app.getMapManager().getLocationManager().requestLocationUpdates(this);

		app.getMapManager().start();

		// if (!SIMUtils.isSimCardReady(this)) {
		// if (locationManager != null) {
		// locationManager.start();
		// }
		// }

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

		app.getMapManager().stop();
		// if (locationManager != null) {
		// locationManager.stop();
		// }

		unregisterReceiver(myreceiver);
		super.onPause();
	}

	@Override
	protected void mappingComponents() {
		super.mappingComponents();
		layPoints = (PointBar) findViewById(R.id.layPoints);
		tvAddress = (TextView) findViewById(R.id.tvAddress);
		gButtons = (ScrollLayout) findViewById(R.id.gButtons);
		ivArr = (ImageView) findViewById(R.id.ivArr);
		ivSplit = (ImageView) findViewById(R.id.ivSplit);
	}

	@Override
	protected void init() {

		super.init();
		// gButtons.setSpacing(getSpacing());
		initGrid9();
		gButtons.setToScreen(1);
		layPoints.setPoint(1);
		tvName.setOnClickListener(this);
		btnRight.setVisibility(View.VISIBLE);
		btnRight.setOnClickListener(this);
		gButtons.setOnScreenChangeListener(this);
	}

	private void initGrid9() {

		// listWelcome = new ArrayList<GridPage>();

		List<PageItem[]> pages = PageUtils.buildPages(this);

		gButtons.addView(new GridPageSearch(this));

		for (int i = 0; i < pages.size(); i++) {

			RelativeLayout lay = new RelativeLayout(this);
			lay.setBackgroundColor(0x00000000);
			lay.setOnClickListener(new OnClickListener() {

				@Override
				public void onClick(View v) {
					int count = gButtons.getChildCount();
					for (int i = 0; i < count; i++) {
						((GridPage4x4) ((RelativeLayout) gButtons.getChildAt(i))
								.getChildAt(0)).setEditStatus(false);
					}
				}
			});
			lay.setLayoutParams(new RelativeLayout.LayoutParams(
					LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
			gButtons.addView(lay);

			GridPage4x4 gp = new GridPage4x4(this);
			RelativeLayout.LayoutParams lp = new RelativeLayout.LayoutParams(
					LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT);
			lp.addRule(RelativeLayout.CENTER_IN_PARENT, 1);
			gp.setLayoutParams(lp);
			gp.setButtonsItem(pages.get(i));
			gp.setButtonClickEvent(this);
			gp.setButtonLongClickEvent(this);
			gp.setDeleteButtonClickEvent(this);
			lay.addView(gp);
		}

		layPoints.setPointCount(gButtons.getChildCount());

		// welcomeAdapter = new WelcomeAdapter(listWelcome);
		// gButtons.setAdapter(welcomeAdapter);
	}

	@Override
	public void onClick(View v) {

		switch (v.getId()) {
		case R.id.tvName:
			if (menu == null) {
				menu = new PopupMenuDialog(this, R.style.dialog);
				menu.setCanceledOnTouchOutside(true);
				menu.setOnCancelListener(new OnCancelListener() {

					@Override
					public void onCancel(DialogInterface dialog) {
						menu = null;

					}
				});
				menu.getButton(0).setOnClickListener(this);
				menu.getButton(1).setOnClickListener(this);
			}

			menu.show();

			break;
		case R.id.btnRight:

			MainApplication app = (MainApplication) getApplication();
			app.getMapManager().stop();
			app.getMapManager().start();

			break;

		case R.id.diagBtn1:
			menu.cancel();
			Intent inSettings = new Intent(this, SettingsActivity.class);
			startActivityForResult(inSettings, 0);
			break;
		case R.id.diagBtn2:
			menu.cancel();
			setEditMode(false);
			AlertDialogEx.showAlertDialogEx(this,
					getString(R.string.add_keyword),
					getString(R.string.add_keyword_hint),
					getString(R.string.ok), new DialogButtonClickListener() {

						@Override
						public void onClick(View v, String text) {

							addKeyword(text);

						}
					}, getString(R.string.cancel), null);
			break;
		}

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
			if (tvAddress.getText().toString().equals("")) {
				tvAddress.setText(R.string.addressing);
			}

			getAddressByGeo(location.getLatitude(), location.getLongitude());
		}
	}

	private void getAddressByGeo(final double lat, final double lng) {

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					tvAddress.setText(GlobalInstance.address);
					if (GlobalInstance.address.equals("")) {
						tvAddress.setText(R.string.cannot_get_address);
					}
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

	public void doCellLocation() {
		CellInfoManager cellManager = new CellInfoManager(this);
		WifiInfoManager wifiManager = new WifiInfoManager(this);
		CellLocationManager locationManager = new CellLocationManager(this,
				cellManager, wifiManager) {
			@Override
			public void onLocationChanged() {

				GlobalInstance.point = new GeoPoint(
						(int) (this.latitude() * 1e6),
						(int) (this.longitude() * 1e6));
				GlobalInstance.pointOri = new GeoPointOri(this.latitude(),
						this.longitude());
				tvAddress.setText(R.string.addressing);

				getAddressByGeo(this.latitude(), this.longitude());
			}
		};
		locationManager.start();
	}

	class MapReceiver extends BroadcastReceiver {

		@Override
		public void onReceive(Context context, Intent intent) {
			tvAddress.setText(R.string.network_error);
			Toast.makeText(context, R.string.network_error, Toast.LENGTH_LONG)
					.show();
		}
	}

	private MapReceiver myreceiver = new MapReceiver();
	private IntentFilter mapFilter = new IntentFilter(
			MainApplication.NETWORK_ERROR_ACTION);

	@Override
	public boolean onLongClick(View v) {
		setEditMode(true);

		return true;
	}

	@Override
	public boolean onKeyDown(int keyCode, KeyEvent event) {
		if (keyCode == KeyEvent.KEYCODE_BACK) {
			boolean edit = getEditMode();
			if (edit) {
				setEditMode(false);
				return true;
			}
		}
		return super.onKeyDown(keyCode, event);
	}

	@Override
	public void onDeleteClick(View v, String tag) {
		if (PageUtils.isLockedItem(this, tag)) {
			return;
		}
		int currScreen = gButtons.getCurScreen();
		List<String> currList = Config.getKeywordsList(this);
		currList.remove(tag);
		Config.saveKeywordList(this, currList);

		refresh();
		gButtons.setToScreen(currScreen);
		onLongClick(null);
	}

	public void addKeyword(String keyword) {
		int currScreen = gButtons.getCurScreen();
		List<String> currList = Config.getKeywordsList(this);
		currList.add(keyword);
		Config.saveKeywordList(this, currList);

		refresh();
		gButtons.setToScreen(currScreen);

	}

	@Override
	public void onKeywordClick(View v, String tag) {
		if (GlobalInstance.point == null) {
			Toast.makeText(this, R.string.no_location_found, Toast.LENGTH_LONG)
					.show();
			return;
		}
		Intent inList = new Intent(this, PoiListActivity.class);
		inList.putExtra("keyword", tag);
		startActivity(inList);

	}

	public void setEditMode(boolean edit) {
		int count = gButtons.getChildCount();
		for (int i = 0; i < count; i++) {
			if (((RelativeLayout) gButtons.getChildAt(i)).getChildAt(0) instanceof GridPage4x4) {
				((GridPage4x4) ((RelativeLayout) gButtons.getChildAt(i))
						.getChildAt(0)).setEditStatus(edit);
			}

		}

	}

	public boolean getEditMode() {
		boolean edit = ((GridPage4x4) ((RelativeLayout) gButtons.getChildAt(1))
				.getChildAt(0)).getEditStatus();
		return edit;
	}

	@Override
	public void onScreenChange(int screen) {
		layPoints.setPoint(screen);

		if (screen == 0) {
			ivArr.setVisibility(View.INVISIBLE);
			ivSplit.setVisibility(View.INVISIBLE);
			btnRight.setVisibility(View.INVISIBLE);
			tvName.setOnClickListener(null);
			inputMgr.showSoftInput(
					((GridPageSearch) gButtons.getChildAt(0)).getEdit(),
					InputMethodManager.SHOW_IMPLICIT);
		} else {
			ivArr.setVisibility(View.VISIBLE);
			ivSplit.setVisibility(View.VISIBLE);
			btnRight.setVisibility(View.VISIBLE);
			tvName.setOnClickListener(this);
			inputMgr.hideSoftInputFromWindow(
					getCurrentFocus().getWindowToken(),
					InputMethodManager.HIDE_NOT_ALWAYS);

		}

	}

}
