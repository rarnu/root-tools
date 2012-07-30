package com.rarnu.tools.root;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.List;

import org.apache.http.protocol.HTTP;

import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AbsListView;
import android.widget.Button;
import android.widget.GridView;

import com.rarnu.tools.root.adapter.WelcomeAdapter;
import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.api.MobileApi;
import com.rarnu.tools.root.base.BaseActivity;
import com.rarnu.tools.root.common.RTConfig;
import com.rarnu.tools.root.common.RTConsts;
import com.rarnu.tools.root.comp.AlertDialogEx;
import com.rarnu.tools.root.comp.WelcomeButton;
import com.rarnu.tools.root.utils.BusyboxUtils;
import com.rarnu.tools.root.utils.DeviceUtils;
import com.rarnu.tools.root.utils.DirHelper;
import com.rarnu.tools.root.utils.GoogleUtils;
import com.rarnu.tools.root.utils.MemorySpecialList;
import com.rarnu.tools.root.utils.NetworkUtils;
import com.rarnu.tools.root.utils.PingUtils;
import com.rarnu.tools.root.utils.UIUtils;
import com.rarnu.tools.root.utils.root.RootUtils;

public class WelcomeActivity extends BaseActivity implements OnClickListener {

	// [region] consts
	private static final int ID_BUTTON_0 = 1001;
	private static final int ID_BUTTON_1 = 1002;
	private static final int ID_BUTTON_2 = 1003;
	private static final int ID_BUTTON_3 = 1004;
	private static final int ID_BUTTON_4 = 1005;
	private static final int ID_BUTTON_5 = 1006;
	private static final int ID_BUTTON_6 = 1007;
	private static final int ID_BUTTON_7 = 1008;
	private static final int ID_BUTTON_8 = 1009;
	// [/region]

	// [region] field define
	GridView gdButtons;
	WelcomeButton[] btnFunc;
	Button btnAbout, btnFeedback, btnRecommand;
	// [/region]

	// [region] variable define
	WelcomeAdapter welcomeAdapter = null;
	List<WelcomeButton> listWelcome = null;

	// [/region]

	// [region] life circle
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		if (!DirHelper.isSDCardExists()) {
			AlertDialogEx.showAlertDialogEx(this, getString(R.string.hint),
					getString(R.string.no_sdcard_found),
					getString(R.string.ok),
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

		initDeviceInfo();
		LogApi.logAppStart();
		RootUtils.mountRW();

		UIUtils.initDisplayMetrics(getWindowManager());
		getWindowManager().getDefaultDisplay()
				.getMetrics(GlobalInstance.metric);
		GlobalInstance.density = GlobalInstance.metric.density;
		GlobalInstance.pm = getPackageManager();

		DirHelper.makeDir();
		setContentView(R.layout.layout_welcome);
		init();
		loadExcludeListT();
		showFunctionalEnabledTags();
		loadNetworkStatus();

		if (GlobalInstance.isFirstStart) {
			LogApi.logAppFirstStart();
			GlobalInstance.isFirstStart = false;
			RTConfig.setFirstStart(this, GlobalInstance.isFirstStart);
		}
		getUpdateInfo();
	}

	@Override
	protected void onDestroy() {
		LogApi.logAppStop();
		super.onDestroy();
	}

	// [/region]

	// [region] init

	@Override
	public void init() {
		initConfig();
		mappingTitle();
		mappingComp();
		initSearchBar();
		initTitle();
		initGrid9();
		initEvents();
	}

	@Override
	public void initSearchBar() {

	}

	private void initDeviceInfo() {
		GlobalInstance.deviceId = DeviceUtils.getDeviceUniqueId(this);
		GlobalInstance.module = DeviceUtils
				.getBuildProp(DeviceUtils.RO_PRODUCT_MODEL);
		GlobalInstance.osVersion = DeviceUtils
				.getBuildProp(DeviceUtils.RO_BUILD_VERSION_RELEASE);
		GlobalInstance.mail = GoogleUtils.getGoogleAccount();
		GlobalInstance.buildDescription = DeviceUtils
				.getBuildProp(DeviceUtils.RO_BUILD_DESCRIPTION);

		try {
			GlobalInstance.deviceId = URLEncoder.encode(
					GlobalInstance.deviceId, HTTP.UTF_8);
			GlobalInstance.module = URLEncoder.encode(GlobalInstance.module,
					HTTP.UTF_8);
			GlobalInstance.osVersion = URLEncoder.encode(
					GlobalInstance.osVersion, HTTP.UTF_8);
			GlobalInstance.mail = URLEncoder.encode(GlobalInstance.mail,
					HTTP.UTF_8);
			GlobalInstance.buildDescription = URLEncoder.encode(
					GlobalInstance.buildDescription, HTTP.UTF_8);
		} catch (UnsupportedEncodingException e) {

		}

	}

	private void initConfig() {
		GlobalInstance.isFirstStart = RTConfig.getFirstStart(this);
		GlobalInstance.allowDeleteLevel0 = RTConfig.getAllowDeleteLevel0(this);
		GlobalInstance.alsoDeleteData = RTConfig.getAlsoDeleteData(this);
		GlobalInstance.backupBeforeDelete = RTConfig
				.getBackupBeforeDelete(this);
		GlobalInstance.overrideBackuped = RTConfig.getOverrideBackuped(this);
		GlobalInstance.reinstallApk = RTConfig.getReinstallApk(this);
		GlobalInstance.killProcessBeforeClean = RTConfig
				.getKillProcessBeforeClean(this);
		GlobalInstance.nameServer = RTConfig.getNameServer(this);
	}

	@Override
	public void mappingComp() {
		// tbTitle = (TitleBar) findViewById(R.id.tbTitle);
		gdButtons = (GridView) findViewById(R.id.gdButtons);
		btnAbout = (Button) findViewById(R.id.btnAbout);
		btnFeedback = (Button) findViewById(R.id.btnFeedback);
		btnRecommand = (Button) findViewById(R.id.btnRecommand);
	}

	@Override
	public void initTitle() {
		tvName.setText(R.string.app_name);
		btnRight.setText(R.string.settings);
		btnRight.setVisibility(View.VISIBLE);
	}

	private void initGrid9() {

		btnFunc = new WelcomeButton[9];
		btnFunc[0] = buildButton(ID_BUTTON_0, R.string.func1);
		btnFunc[1] = buildButton(ID_BUTTON_1, R.string.func2);
		btnFunc[2] = buildButton(ID_BUTTON_2, R.string.func3);
		btnFunc[3] = buildButton(ID_BUTTON_3, R.string.func4);
		btnFunc[4] = buildButton(ID_BUTTON_4, R.string.func5);
		btnFunc[5] = buildButton(ID_BUTTON_5, R.string.func6);
		btnFunc[6] = buildButton(ID_BUTTON_6, R.string.func7);
		btnFunc[7] = buildButton(ID_BUTTON_7, R.string.func8);
		btnFunc[8] = buildButton(ID_BUTTON_8, R.string.func9);

		listWelcome = new ArrayList<WelcomeButton>();
		for (int i = 0; i < btnFunc.length; i++) {
			listWelcome.add(btnFunc[i]);
		}

		welcomeAdapter = new WelcomeAdapter(listWelcome);
		gdButtons.setAdapter(welcomeAdapter);
	}

	@Override
	public void initEvents() {
		// tbTitle.getRightButton().setOnClickListener(this);
		btnRight.setOnClickListener(this);
		for (int i = 0; i < btnFunc.length; i++) {
			btnFunc[i].getButton().setOnClickListener(this);
		}
		btnAbout.setOnClickListener(this);
		btnFeedback.setOnClickListener(this);
		btnRecommand.setOnClickListener(this);
	}

	// [/region]

	// [region] events
	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnRight:
			startActivity(SettingsActivity.class);
			break;
		case ID_BUTTON_0:
			startActivityForResult(SysappMainActivity.class,
					RTConsts.REQCODE_SYSAPP);
			break;
		case ID_BUTTON_1:
			startActivity(DataappMainActivity.class);
			break;
		case ID_BUTTON_2:
			startActivity(CompMainActivity.class);
			break;
		case ID_BUTTON_3:
			startActivity(MemMainActivity.class);
			break;
		case ID_BUTTON_4:
			startActivity(HostMainActivity.class);
			break;
		case ID_BUTTON_5:
			startActivity(CleanCacheMainActivity.class);
			break;
		case ID_BUTTON_6:
			startActivityForResult(BusyboxActivity.class,
					RTConsts.REQCODE_BUSYBOX);
			break;
		case ID_BUTTON_7:
			startActivity(MockGpsActivity.class);
			break;
		case ID_BUTTON_8:
			startActivity(MoreMainActivity.class);
			break;
		case R.id.btnAbout:
			startActivity(AboutActivity.class);
			break;
		case R.id.btnFeedback:
			startActivity(UserFeedbackActivity.class);
			break;
		case R.id.btnRecommand:
			startActivity(RecommandActivity.class);
			break;
		}

	}

	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent data) {
		if (resultCode != RESULT_OK) {
			return;
		}

		switch (requestCode) {

		case RTConsts.REQCODE_SYSAPP:
		case RTConsts.REQCODE_BUSYBOX:
			showFunctionalEnabledTags();
			break;
		}
	}

	// [/region]

	// [region] business logic

	private WelcomeButton buildButton(int id, int text) {
		WelcomeButton btn = new WelcomeButton(this);
		btn.setText(getString(text));
		btn.setButtonId(id);
		btn.setImageStyle(WelcomeButton.BS_NONE);
		btn.setLayoutParams(new AbsListView.LayoutParams(UIUtils.dipToPx(96),
				UIUtils.dipToPx(96)));
		return btn;
	}

	private void startActivity(Class<?> cls) {
		Intent intent = new Intent(this, cls);
		startActivity(intent);
	}

	private void startActivityForResult(Class<?> cls, int requestCode) {
		Intent intent = new Intent(this, cls);
		startActivityForResult(intent, requestCode);
	}

	private void showBusyboxTag() {
		boolean ready = BusyboxUtils.isSuBusyboxReady();
		btnFunc[6].setImageStyle(ready ? WelcomeButton.BS_NONE
				: WelcomeButton.BS_WARNING);
	}

	private void loadExcludeListT() {
		new Thread(new Runnable() {
			@Override
			public void run() {
				MemorySpecialList.loadExcludeList();
			}
		}).start();
	}

	private void loadNetworkStatus() {
		new Thread(new Runnable() {

			@Override
			public void run() {
				GlobalInstance.loadingNetwork = true;
				GlobalInstance.networkInfo = NetworkUtils
						.getNetworkInfo(WelcomeActivity.this);
				GlobalInstance.networkSpeed = PingUtils
						.testNetworkSpeed(WelcomeActivity.this);
				GlobalInstance.loadingNetwork = false;
			}
		}).start();
	}

	private void showFunctionalEnabledTags() {

		boolean isRooted = RootUtils.hasSu();
		boolean isWrongRooted = RootUtils.isWrongRoot();
		if (isWrongRooted) {
			isRooted = false;
		}

		boolean isBusyboxInstalled = RootUtils.hasBusybox();

		resetFuncButtons();

		if (!isRooted) {
			for (int i = 0; i < btnFunc.length - 1; i++) {
				btnFunc[i].setImageStyle(WelcomeButton.BS_BANNED);
			}
			return;
		}

		if (!isBusyboxInstalled) {
			btnFunc[0].setImageStyle(WelcomeButton.BS_WARNING);
			btnFunc[1].setImageStyle(WelcomeButton.BS_WARNING);
			btnFunc[3].setImageStyle(WelcomeButton.BS_WARNING);
			btnFunc[4].setImageStyle(WelcomeButton.BS_WARNING);
			btnFunc[5].setImageStyle(WelcomeButton.BS_WARNING);
			btnFunc[7].setImageStyle(WelcomeButton.BS_WARNING);
			showBusyboxTag();
		}

	}

	private void resetFuncButtons() {
		for (int i = 0; i < btnFunc.length; i++) {
			btnFunc[i].setImageStyle(WelcomeButton.BS_NONE);
		}
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
	// [/region]

}
