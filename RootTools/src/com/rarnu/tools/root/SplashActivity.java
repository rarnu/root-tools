package com.rarnu.tools.root;

import java.net.URLEncoder;
import java.util.Timer;
import java.util.TimerTask;

import org.apache.http.protocol.HTTP;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.os.Bundle;
import android.view.KeyEvent;

import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.utils.DeviceUtils;
import com.rarnu.tools.root.utils.DirHelper;
import com.rarnu.tools.root.utils.GoogleUtils;
import com.rarnu.tools.root.utils.UIUtils;

public class SplashActivity extends Activity {

	// [region] life circle
	@Override
	public void onCreate(Bundle savedInstanceState) {

		super.onCreate(savedInstanceState);
		UIUtils.initDisplayMetrics(getWindowManager());

		if (!DirHelper.isSDCardExists()) {

			new AlertDialog.Builder(this)
					.setTitle(R.string.hint)
					.setMessage(R.string.no_sdcard_found)
					.setPositiveButton(R.string.ok,
							new DialogInterface.OnClickListener() {

								@Override
								public void onClick(DialogInterface dialog,
										int which) {
									finish();

								}
							}).show();

			return;
		}

		setContentView(R.layout.layout_splash);

		initDeviceInfo();
		LogApi.logAppStart();

		getWindowManager().getDefaultDisplay()
				.getMetrics(GlobalInstance.metric);
		GlobalInstance.density = GlobalInstance.metric.density;
		GlobalInstance.pm = getPackageManager();

		DirHelper.makeDir();

		final Timer tmrClose = new Timer();
		tmrClose.schedule(new TimerTask() {

			@Override
			public void run() {
				tmrClose.cancel();
				finish();
				startMainActivity();
			}
		}, 2000);

	}

	private void startMainActivity() {
		Intent inMain = new Intent(this, MainActivity.class);
		inMain.setFlags(Intent.FLAG_ACTIVITY_NO_USER_ACTION);
		startActivity(inMain);
	}

	// [/region]

	// [region] events
	@Override
	public boolean onKeyDown(int keyCode, KeyEvent event) {
		return true;
	}

	// [/region]

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
		} catch (Exception e) {
			GlobalInstance.deviceId = "0";
		}

		try {
			GlobalInstance.module = URLEncoder.encode(GlobalInstance.module,
					HTTP.UTF_8);
		} catch (Exception e) {
			GlobalInstance.module = "0";
		}
		try {
			GlobalInstance.osVersion = URLEncoder.encode(
					GlobalInstance.osVersion, HTTP.UTF_8);
		} catch (Exception e) {
			GlobalInstance.osVersion = "0";
		}
		try {
			GlobalInstance.mail = URLEncoder.encode(GlobalInstance.mail,
					HTTP.UTF_8);
		} catch (Exception e) {
			GlobalInstance.mail = "";
		}
		try {
			GlobalInstance.buildDescription = URLEncoder.encode(
					GlobalInstance.buildDescription, HTTP.UTF_8);
		} catch (Exception e) {
			GlobalInstance.buildDescription = "";
		}

	}

}
