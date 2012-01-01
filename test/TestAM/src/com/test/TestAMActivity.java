package com.test;

import java.io.File;

import android.app.Activity;
import android.content.ComponentName;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageParser;
import android.content.pm.PackageManager.NameNotFoundException;
import android.os.Bundle;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;

public class TestAMActivity extends Activity implements OnClickListener {

	Button btnEnable, btnDisable, btnSendMsg, btnQuery;
	EditText edtPackgeName;
	TextView tvResult;
	PackageManager pm;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.main);

		pm = getPackageManager();

		btnEnable = (Button) findViewById(R.id.btnEnable);
		btnDisable = (Button) findViewById(R.id.btnDisable);
		btnSendMsg = (Button) findViewById(R.id.btnSendMsg);
		btnQuery = (Button) findViewById(R.id.btnQuery);
		tvResult = (TextView) findViewById(R.id.tvResult);
		edtPackgeName = (EditText) findViewById(R.id.edtPackgeName);

		btnEnable.setOnClickListener(this);
		btnDisable.setOnClickListener(this);
		btnSendMsg.setOnClickListener(this);
		btnQuery.setOnClickListener(this);

	}

	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnEnable:
			doEnabledReceiver();
			break;
		case R.id.btnDisable:
			doDisableReceiver();
			break;
		case R.id.btnSendMsg:
			doSendMessage();
			break;
		case R.id.btnQuery:
			doQueryReceiver();
			break;
		}

	}

	public void doEnabledReceiver() {

		PackageInfo pi;

		try {

			pi = pm.getPackageInfo("com.test", PackageManager.GET_RECEIVERS);
			Log.e("AI", pi.packageName);
			Log.e("AI", String.valueOf(pi.receivers.length));
			for (ActivityInfo ai : pi.receivers) {

				Log.e("AI", ai.name);
			}
			pm.setComponentEnabledSetting(new ComponentName(pi.packageName,
					pi.receivers[0].name),
					PackageManager.COMPONENT_ENABLED_STATE_ENABLED,
					PackageManager.DONT_KILL_APP);

		} catch (NameNotFoundException e) {

			e.printStackTrace();
		}

	}

	public void doDisableReceiver() {
		PackageInfo pi;
		try {
			pi = pm.getPackageInfo("com.test", PackageManager.GET_RECEIVERS);
			Log.e("AI", pi.packageName);
			Log.e("AI", String.valueOf(pi.receivers.length));
			for (ActivityInfo ai : pi.receivers) {

				Log.e("AI", ai.name);
				Log.e("AI", ai.packageName);

				Log.e("AI", ai.processName);
				Log.e("AI", ai.targetActivity);
				Log.e("AI", ai.taskAffinity);
				Log.e("AI", ai.metaData.toString());

			}
			pm.setComponentEnabledSetting(new ComponentName(pi.packageName,
					pi.receivers[0].name),
					PackageManager.COMPONENT_ENABLED_STATE_DISABLED,
					PackageManager.DONT_KILL_APP);
		} catch (NameNotFoundException e) {

			e.printStackTrace();
		}
	}

	public void doSendMessage() {
		Intent inMsg = new Intent("COM.SNDA.TEST.MESSAGE");
		sendBroadcast(inMsg);
	}

	public void doQueryReceiver() {
		tvResult.setText("");
		String packName = edtPackgeName.getText().toString();
		PackageInfo packInfo = null;
		try {
			packInfo = pm.getPackageInfo(packName, 0);
		} catch (NameNotFoundException e) {
		}

		if (packInfo == null) {
			return;
		}

		String fileAbsPath = packInfo.applicationInfo.publicSourceDir;
		PackageParser packageParser = new PackageParser(fileAbsPath);
		File sourceFile = new File(fileAbsPath);
		DisplayMetrics metrics = new DisplayMetrics();
		metrics.setToDefaults();
		PackageParser.Package pkg = packageParser.parsePackage(sourceFile,
				fileAbsPath, metrics, PackageParser.PARSE_IS_SYSTEM);
		String resultStr = "";
		for (PackageParser.Activity a : pkg.receivers) {
			for (PackageParser.ActivityIntentInfo aii : a.intents) {
				if (aii.countActions() > 0) {
					for (int i = 0; i < aii.countActions(); i++) {
						Log.e("AI", "Action: " + aii.getAction(i));
						resultStr += aii.getAction(i) + "\n";
					}
				}

			}

		}
		tvResult.setText(resultStr);

	}
}
