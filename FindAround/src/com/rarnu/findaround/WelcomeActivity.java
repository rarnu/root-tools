package com.rarnu.findaround;

import java.util.ArrayList;
import java.util.List;

import android.annotation.SuppressLint;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.os.SystemProperties;
import android.view.KeyEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.OnKeyListener;
import android.view.View.OnLongClickListener;
import android.view.inputmethod.InputMethodManager;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ArrayAdapter;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.RelativeLayout;
import android.widget.RelativeLayout.LayoutParams;
import android.widget.TextView;
import android.widget.Toast;

import com.rarnu.findaround.api.MobileApi;
import com.rarnu.findaround.base.BaseActivity;
import com.rarnu.findaround.common.Config;
import com.rarnu.findaround.common.DeviceUtils;
import com.rarnu.findaround.common.PageItem;
import com.rarnu.findaround.common.PageUtils;
import com.rarnu.findaround.common.UIUtils;
import com.rarnu.findaround.comp.AlertDialogEx;
import com.rarnu.findaround.comp.GridPage4x4;
import com.rarnu.findaround.comp.GridPage4x4.OnDeleteClickListener;
import com.rarnu.findaround.comp.GridPage4x4.OnKeywordClickListener;
import com.rarnu.findaround.comp.GridPageSearch;
import com.rarnu.findaround.comp.PointBar;
import com.rarnu.findaround.comp.PopupMenuDialog;
import com.rarnu.findaround.comp.ScrollLayout;
import com.rarnu.findaround.comp.ScrollLayout.OnScreenChangeListener;
import com.rarnu.findaround.service.SearchService;

@SuppressLint("HandlerLeak")
public class WelcomeActivity extends BaseActivity implements OnClickListener,
		OnLongClickListener, OnDeleteClickListener, OnKeywordClickListener,
		OnScreenChangeListener, OnItemClickListener {

	ScrollLayout gButtons;
	TextView tvAddress;
	PointBar layPoints;
	PopupMenuDialog menu;
	InputMethodManager inputMgr;
	GridPageSearch pageSearch;
	RelativeLayout layBottom;

	List<String> listHistory = null;
	ListView lvHistory = null;
	ArrayAdapter<String> adapterHistory = null;

	boolean isForceGpu = false;

	// CellLocationManager locationManager = null;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		UIUtils.initDisplayMetrics(getWindowManager());

		isForceGpu = isForceGpuRendering();
		if (isForceGpu) {
			AlertDialogEx.showAlertDialogEx(this, getString(R.string.hint),
					getString(R.string.gpu_40), getString(R.string.ok),
					new AlertDialogEx.DialogButtonClickListener() {
						@Override
						public void onClick(View v) {
							finish();
						}
					}, null, null);
			return;
		}

		Intent inSplash = new Intent(this, SplashActivity.class);
		startActivity(inSplash);

		GlobalInstance.pm = getPackageManager();
		GlobalInstance.search = new SearchService(this,
				(MainApplication) getApplication());

		inputMgr = (InputMethodManager) getSystemService(Context.INPUT_METHOD_SERVICE);
		setContentView(R.layout.welcome);
		init();
		GlobalInstance.search.locate();
		getUpdateInfo();
		// if (!SIMUtils.isSimCardReady(this)) {
		// doCellLocation();
		// }
	}

	protected void refresh() {
		// setContentView(R.layout.welcome);

		// init();
		// tvAddress.setText(GlobalInstance.address);
		initGrid9();
	}

	@Override
	protected void onDestroy() {
		if (!isForceGpu) {
			GlobalInstance.point = null;
			Config.saveHistoryList(this, listHistory);
		}
		super.onDestroy();
	}

	@Override
	protected void onResume() {
		if (!isForceGpu) {
			registerReceiver(myreceiver, mapFilter);
			registerReceiver(locationReceiver, locationFilter);
			GlobalInstance.search.locate();
		}

		super.onResume();

	}

	@Override
	protected void onPause() {
		if (!isForceGpu) {
			GlobalInstance.search.stop();
			unregisterReceiver(locationReceiver);
			unregisterReceiver(myreceiver);
			setEditMode(false);
		}
		super.onPause();

	}

	@Override
	protected void mappingComponents() {
		super.mappingComponents();
		pageSearch = new GridPageSearch(this);
		layPoints = (PointBar) findViewById(R.id.layPoints);
		layBottom = (RelativeLayout) findViewById(R.id.layBottom);
		tvAddress = (TextView) findViewById(R.id.tvAddress);
		gButtons = (ScrollLayout) findViewById(R.id.gButtons);
	}

	@Override
	protected void init() {

		super.init();
		// gButtons.setSpacing(getSpacing());

		initGrid9();
		gButtons.setToScreen(1);
		layPoints.setPoint(1);
		// tvName.setOnClickListener(this);
		// btnRight.setVisibility(View.VISIBLE);
		btnRight.setOnClickListener(this);
		gButtons.setOnScreenChangeListener(this);
		pageSearch.getEdit().setOnKeyListener(new OnKeyListener() {

			@Override
			public boolean onKey(View v, int keyCode, KeyEvent event) {
				if (event.getAction() == KeyEvent.ACTION_DOWN) {
					if (keyCode == KeyEvent.KEYCODE_ENTER) {
						String tag = ((EditText) v).getText().toString();
						((EditText) v).setText("");
						if (tag != null) {
							tag = tag.trim();
							if (!tag.equals("")) {
								onKeywordClick(v, tag);
								addHistory(tag);
								gButtons.snapToScreen(1);
							}
						}

						return true;
					}
				}
				return false;
			}
		});

		lvHistory = pageSearch.getListView();
		lvHistory.setOnItemClickListener(this);
		final Handler hHistory = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					lvHistory.setAdapter(adapterHistory);
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				listHistory = Config.getHistoryList(WelcomeActivity.this);

				adapterHistory = new ArrayAdapter<String>(WelcomeActivity.this,
						R.layout.history_item, R.id.tvHistoryItem, listHistory);

				hHistory.sendEmptyMessage(1);
			}
		}).start();

		layBottom.setOnClickListener(this);

	}

	private void initGrid9() {
		gButtons.removeAllViews();
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
		case R.id.btnRight:
			if (gButtons.getCurScreen() == 0) {
				listHistory.clear();
				adapterHistory.notifyDataSetChanged();
			} else {
				Intent inSettings = new Intent(this, SettingsActivity.class);
				startActivity(inSettings);
			}
			break;
		case R.id.layBottom:
			GlobalInstance.search.locate();
			break;
		}
	}

	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent data) {
		if (resultCode != RESULT_OK) {
			return;
		}
		switch (requestCode) {
		case 0:
			addKeyword(data.getStringExtra("keyword"));
			break;
		}
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

		if (tag == null) {
			return;
		}
		tag = tag.trim();
		if (tag.equals("")) {
			return;
		}
		addHistory(tag);
		Intent inList = new Intent(this, PoiListActivity.class);
		inList.putExtra("keyword", tag);
		inList.putExtra("exists", PageUtils.isKeywordExists(tag));
		startActivityForResult(inList, 0);

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
			setEditMode(false);
			btnRight.setBackgroundResource(R.drawable.btn_trash_style);
			// inputMgr.showSoftInput(pageSearch.getEdit(),
			// InputMethodManager.SHOW_IMPLICIT);
		} else {
			btnRight.setBackgroundResource(R.drawable.btn_settings_style);
			inputMgr.hideSoftInputFromWindow(
					getCurrentFocus().getWindowToken(),
					InputMethodManager.HIDE_NOT_ALWAYS);

		}

	}

	private void addHistory(String tag) {
		if (listHistory == null) {
			listHistory = new ArrayList<String>();
		}
		if (listHistory.indexOf(tag) != -1) {
			listHistory.remove(tag);
		}
		listHistory.add(0, tag);
		adapterHistory.notifyDataSetChanged();
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		onKeywordClick(view, listHistory.get(position));
	}

	private void getUpdateInfo() {

		new Thread(new Runnable() {

			@Override
			public void run() {
				int verCode = DeviceUtils
						.getAppVersionCode(WelcomeActivity.this);
				GlobalInstance.updateInfo = MobileApi.checkUpdate(verCode);
			}
		}).start();
	}

	public static boolean isForceGpuRendering() {
		final String HARDWARE_UI_PROPERTY = "persist.sys.ui.hw";
		try {
			return SystemProperties.getBoolean(HARDWARE_UI_PROPERTY, false);
		} catch (Exception e) {
			return false;
		}
	}
}
