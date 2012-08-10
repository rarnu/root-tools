package com.rarnu.findaround;

import java.util.List;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.DialogInterface;
import android.content.DialogInterface.OnCancelListener;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.OnKeyListener;
import android.view.View.OnLongClickListener;
import android.view.inputmethod.InputMethodManager;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.RelativeLayout.LayoutParams;
import android.widget.TextView;
import android.widget.Toast;

import com.rarnu.findaround.base.BaseActivity;
import com.rarnu.findaround.common.Config;
import com.rarnu.findaround.common.PageItem;
import com.rarnu.findaround.common.PageUtils;
import com.rarnu.findaround.common.UIUtils;
import com.rarnu.findaround.comp.GridPage4x4;
import com.rarnu.findaround.comp.GridPage4x4.OnDeleteClickListener;
import com.rarnu.findaround.comp.GridPage4x4.OnKeywordClickListener;
import com.rarnu.findaround.comp.GridPageSearch;
import com.rarnu.findaround.comp.LineEditText;
import com.rarnu.findaround.comp.PointBar;
import com.rarnu.findaround.comp.PopupMenuDialog;
import com.rarnu.findaround.comp.ScrollLayout;
import com.rarnu.findaround.comp.ScrollLayout.OnScreenChangeListener;
import com.rarnu.findaround.service.SearchService;

public class WelcomeActivity extends BaseActivity implements OnClickListener,
		OnLongClickListener, OnDeleteClickListener, OnKeywordClickListener,
		OnScreenChangeListener {

	ScrollLayout gButtons;
	TextView tvAddress;
	PointBar layPoints;
	PopupMenuDialog menu;
	ImageView ivArr, ivSplit;
	InputMethodManager inputMgr;
	GridPageSearch pageSearch;

	// CellLocationManager locationManager = null;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		UIUtils.initDisplayMetrics(getWindowManager());
		GlobalInstance.pm = getPackageManager();
		GlobalInstance.search = new SearchService(this,
				(MainApplication) getApplication());
		inputMgr = (InputMethodManager) getSystemService(Context.INPUT_METHOD_SERVICE);
		setContentView(R.layout.welcome);

		init();

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
		registerReceiver(myreceiver, mapFilter);
		registerReceiver(locationReceiver, locationFilter);
		GlobalInstance.search.locate();
		// if (!SIMUtils.isSimCardReady(this)) {
		// if (locationManager != null) {
		// locationManager.start();
		// }
		// }

		super.onResume();

	}

	@Override
	protected void onPause() {
		GlobalInstance.search.stop();
		unregisterReceiver(locationReceiver);
		unregisterReceiver(myreceiver);

		super.onPause();
	}

	@Override
	protected void mappingComponents() {
		super.mappingComponents();
		pageSearch = new GridPageSearch(this);
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
		pageSearch.getEdit().setOnKeyListener(new OnKeyListener() {

			@Override
			public boolean onKey(View v, int keyCode, KeyEvent event) {
				if (keyCode == KeyEvent.KEYCODE_ENTER) {
					String tag = ((LineEditText) v).getText().toString();
					((LineEditText) v).setText("");
					onKeywordClick(v, tag);
					gButtons.snapToScreen(1);
					addKeyword(tag);
				}
				return false;
			}
		});

		tvAddress.setOnClickListener(this);
	}

	private void initGrid9() {
		List<PageItem[]> pages = PageUtils.buildPages(this);
		gButtons.addView(pageSearch);
		for (int i = 0; i < pages.size(); i++) {
			RelativeLayout lay = new RelativeLayout(this);
			lay.setOnClickListener(new OnClickListener() {
				@Override
				public void onClick(View v) {
					setEditMode(false);
				}
			});
			lay.setLayoutParams(new RelativeLayout.LayoutParams(
					LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
			gButtons.addView(lay);
			GridPage4x4 gp = new GridPage4x4(this);
			gp.setOnClickListener(new OnClickListener() {
				@Override
				public void onClick(View v) {
					setEditMode(false);
				}
			});
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
			GlobalInstance.search.locate();
			break;

		case R.id.diagBtn1:
			menu.cancel();
			setEditMode(false);
			Intent inKeyword = new Intent(this, KeywordsActivity.class);
			startActivityForResult(inKeyword, 0);
			break;
		case R.id.diagBtn2:
			menu.cancel();
			setEditMode(false);
			Intent inSettings = new Intent(this, SettingsActivity.class);
			startActivity(inSettings);
			break;
		case R.id.tvAddress:
			Intent inStreet = new Intent(this, StreetViewActivity.class);
			startActivity(inStreet);
			break;
		}

	}
	
	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent data) {
		if (resultCode != RESULT_OK) {
			return;
		}
		if (requestCode == 0) {
			// TODO: add result
		}
	}

	// public void doCellLocation() {
	// CellInfoManager cellManager = new CellInfoManager(this);
	// WifiInfoManager wifiManager = new WifiInfoManager(this);
	// CellLocationManager locationManager = new CellLocationManager(this,
	// cellManager, wifiManager) {
	// @Override;
	// public void onLocationChanged() {
	//
	// GlobalInstance.point = new GeoPoint(
	// (int) (this.latitude() * 1e6),
	// (int) (this.longitude() * 1e6));
	// GlobalInstance.pointOri = new GeoPointOri(this.latitude(),
	// this.longitude());
	// tvAddress.setText(R.string.addressing);
	//
	// getAddressByGeo(this.latitude(), this.longitude());
	// }
	// };
	// locationManager.start();
	// }

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

	class LocationReceiver extends BroadcastReceiver {
		@Override
		public void onReceive(Context context, Intent intent) {
			tvAddress.setText(GlobalInstance.address);
			if (GlobalInstance.address.equals("")) {
				tvAddress.setText(R.string.cannot_get_address);
			}
		}
	}

	private LocationReceiver locationReceiver = new LocationReceiver();
	private IntentFilter locationFilter = new IntentFilter(
			MainApplication.ADDRESS_FOUND_ACTION);

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
		if (tag.equals("")) {
			setEditMode(false);
			return;
		}
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
		gButtons.requestFocus();
	}

	public void addKeyword(String keyword) {
		int currScreen = gButtons.getCurScreen();
		List<String> currList = Config.getKeywordsList(this);
		currList.add(keyword);
		Config.saveKeywordList(this, currList);

		refresh();
		gButtons.setToScreen(currScreen);
		gButtons.requestFocus();
	}

	@Override
	public void onKeywordClick(View v, String tag) {
		if (v.getVisibility() != View.VISIBLE) {
			setEditMode(false);
			return;
		}
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
			inputMgr.showSoftInput(pageSearch.getEdit(),
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
