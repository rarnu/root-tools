package com.rarnu.tools.root;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager.NameNotFoundException;
import android.net.NetworkInfo;
import android.net.Uri;
import android.os.Bundle;
import android.os.Environment;
import android.os.Handler;
import android.os.Message;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.Toast;

import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.base.BaseActivity;
import com.rarnu.tools.root.comp.AlertDialogEx;
import com.rarnu.tools.root.utils.ApkUtils;
import com.rarnu.tools.root.utils.DalvikUtils;
import com.rarnu.tools.root.utils.DeviceUtils;

@SuppressLint("HandlerLeak")
public class MoreMainActivity extends BaseActivity implements OnClickListener {

	// [region] const define
	private static final String NS_SUPER_USER_1 = "eu.chainfire.supersu";
	private static final String AS_SUPER_USER_1 = ".MainActivity";
	private static final String NS_SUPER_USER_2 = "com.noshufou.android.su";
	private static final String AS_SUPER_USER_2 = ".Su";
	private static final String NS_SUPER_USER_3 = "com.miui.uac";
	private static final String AS_SUPER_USER_3 = ".Su";

	private static final String NS_ROOT_EXPLORER = "com.speedsoftware.rootexplorer";
	private static final String AS_ROOT_EXPLORER = ".RootExplorer";
	// [/region]

	// [region] field define

	RelativeLayout layMore1, layMore2, layMore3, layMore4, layMore5, layMore6;
	RelativeLayout layMoreHtc;

	// DataProgressBar progressMore;

	// [/region]

	// [region] life circle
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.layout_more);
		init();
		LogApi.logEnterMore();
	}

	// [/region]

	// [region] init
	@Override
	public void init() {
		mappingTitle();
		mappingComp();
		initTitle();
		initSearchBar();
		initEvents();

	}

	@Override
	public void mappingComp() {

		layMore1 = (RelativeLayout) findViewById(R.id.layMore1);
		layMore2 = (RelativeLayout) findViewById(R.id.layMore2);
		layMore3 = (RelativeLayout) findViewById(R.id.layMore3);
		layMore4 = (RelativeLayout) findViewById(R.id.layMore4);
		layMore5 = (RelativeLayout) findViewById(R.id.layMore5);
		layMore6 = (RelativeLayout) findViewById(R.id.layMore6);
		layMoreHtc = (RelativeLayout) findViewById(R.id.layMoreHtc);
		// progressMore = (DataProgressBar) findViewById(R.id.progressMore);
	}

	@Override
	public void initTitle() {
		tvName.setText(R.string.func9_func);
		btnLeft.setText(R.string.back);
		btnLeft.setVisibility(View.VISIBLE);

		// tbTitle.setText(getString(R.string.func9_func));
		// tbTitle.setLeftButtonText(getString(R.string.back));
		// tbTitle.getLeftButton().setVisibility(View.VISIBLE);

	}

	@Override
	public void initSearchBar() {

	}

	@Override
	public void initEvents() {
		btnLeft.setOnClickListener(this);
		layMore1.setOnClickListener(this);
		layMore2.setOnClickListener(this);
		layMore3.setOnClickListener(this);
		layMore4.setOnClickListener(this);
		layMore5.setOnClickListener(this);
		layMore6.setOnClickListener(this);
		layMoreHtc.setOnClickListener(this);
	}

	// [/region]

	// [region] events
	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnLeft:
			finish();
			break;
		case R.id.layMore1:
			doScanMedia();
			break;
		case R.id.layMore2:
			// clean dalvik
			doCleanDalvik();
			break;
		case R.id.layMore3:
			// check network status
			showNetworkStatus();
			break;
		case R.id.layMore4:
			// reboot device
			doReboot();
			break;
		case R.id.layMore5:
			int su = getInstalledSuperuser();

			try {
				switch (su) {
				case 1:
					ApkUtils.gotoApp(this, NS_SUPER_USER_1, AS_SUPER_USER_1);
					break;
				case 2:
					ApkUtils.gotoApp(this, NS_SUPER_USER_2, AS_SUPER_USER_2);
					break;
				case 3:
					ApkUtils.gotoApp(this, NS_SUPER_USER_3, AS_SUPER_USER_3);
					break;
				}

			} catch (Exception e) {
				Toast.makeText(this, R.string.intent_open_error,
						Toast.LENGTH_LONG).show();
			}
			break;
		case R.id.layMore6:
			try {
				ApkUtils.gotoApp(this, NS_ROOT_EXPLORER, AS_ROOT_EXPLORER);
			} catch (Exception e) {
				Toast.makeText(this, R.string.intent_open_error,
						Toast.LENGTH_LONG).show();
			}
			break;
		case R.id.layMoreHtc:
			Intent inHtcRom = new Intent(this, MoreHtcRomActivity.class);
			startActivity(inHtcRom);
			break;
		}
	}

	// [/region]

	// [region] business logic
	private void showNetworkStatus() {

		if (GlobalInstance.loadingNetwork) {
			AlertDialogEx.showAlertDialogEx(this,
					getString(R.string.check_network_status),
					getString(R.string.loading_network_status),
					getString(R.string.ok), null, null, null);
			return;
		}

		String status = getString(R.string.no_connect_found);
		if (GlobalInstance.networkInfo != null) {

			status = String
					.format(getString(R.string.network_status_fmt),
							GlobalInstance.networkInfo.getTypeName(),
							GlobalInstance.networkInfo.getSubtypeName(),
							networkStatusToReadableString(GlobalInstance.networkInfo
									.getState()),

							(GlobalInstance.networkInfo.getExtraInfo() == null ? getString(R.string.not_contained)
									: GlobalInstance.networkInfo.getExtraInfo()),

							(GlobalInstance.networkInfo.isRoaming() ? getString(R.string.yes)
									: getString(R.string.no)),
							(GlobalInstance.networkInfo.isFailover() ? getString(R.string.supported)
									: getString(R.string.unsupported)),
							(GlobalInstance.networkInfo.isAvailable() ? getString(R.string.available)
									: getString(R.string.unavailable)),
							GlobalInstance.networkSpeed);
		}

		AlertDialogEx.showAlertDialogEx(this,
				getString(R.string.check_network_status), status,
				getString(R.string.ok), null, null, null);
	}

	private String networkStatusToReadableString(NetworkInfo.State state) {
		switch (state) {
		case CONNECTED:
			return getString(R.string.network_connected);
		case CONNECTING:
			return getString(R.string.network_connecting);
		case DISCONNECTED:
			return getString(R.string.network_disconnected);
		case DISCONNECTING:
			return getString(R.string.network_disconnecting);
		case SUSPENDED:
			return getString(R.string.network_suspended);
		case UNKNOWN:
			return getString(R.string.network_unknown);

		}
		return getString(R.string.network_unknown);
	}

	private void doScanMedia() {
		LogApi.logScanMedia();
		sendBroadcast(new Intent(Intent.ACTION_MEDIA_SCANNER_SCAN_FILE,
				Uri.parse("file://"
						+ Environment.getExternalStorageDirectory()
								.getAbsolutePath())));
		Log.e("Media Path", Environment.getExternalStorageDirectory()
				.getAbsolutePath());
		Toast.makeText(this, R.string.scan, Toast.LENGTH_LONG).show();
	}

	private void doReboot() {
		AlertDialogEx.showAlertDialogEx(this,
				getString(R.string.reboot_device),
				getString(R.string.confirm_reboot), getString(R.string.ok),
				new AlertDialogEx.DialogButtonClickListener() {

					@Override
					public void onClick(View v) {
						DeviceUtils.reboot();

					}
				}, getString(R.string.cancel), null);
	}

	private void doCleanDalvik() {
		// system@app@TuneIn.apk@classes.dex

		((TextView) layMore2.findViewById(R.id.tvCleanDalvik))
				.setText(R.string.cleaning_cache);
		layMore2.setEnabled(false);

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {

					if (msg.arg1 == -1) {
						Toast.makeText(MoreMainActivity.this,
								R.string.clean_dalvik_fail, Toast.LENGTH_LONG)
								.show();
					} else if (msg.arg1 == 0) {
						Toast.makeText(MoreMainActivity.this,
								R.string.clean_dalvik_0, Toast.LENGTH_LONG)
								.show();
					} else {
						Toast.makeText(
								MoreMainActivity.this,
								String.format(
										getString(R.string.clean_dalvik_succ),
										msg.arg1), Toast.LENGTH_LONG).show();
					}

					((TextView) layMore2.findViewById(R.id.tvCleanDalvik))
							.setText(R.string.clean_dalvik);
					layMore2.setEnabled(true);
				}
				super.handleMessage(msg);
			}
		};
		new Thread(new Runnable() {

			@Override
			public void run() {
				Message msg = new Message();
				msg.what = 1;
				msg.arg1 = DalvikUtils.cleanDalvik();
				h.sendMessage(msg);

			}
		}).start();
	}

	private int getInstalledSuperuser() {

		ApplicationInfo info = null;
		try {
			info = GlobalInstance.pm.getApplicationInfo(NS_SUPER_USER_1, 0);
			if (info != null) {
				return 1;
			}
		} catch (NameNotFoundException e) {

		}
		try {
			info = GlobalInstance.pm.getApplicationInfo(NS_SUPER_USER_2, 0);
			if (info != null) {
				return 2;
			}
		} catch (Exception e) {

		}

		try {
			info = GlobalInstance.pm.getApplicationInfo(NS_SUPER_USER_3, 0);
			if (info != null) {
				return 3;
			}
		} catch (Exception e) {

		}
		return 0;
	}
	// [/region]
}
