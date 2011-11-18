package com.snda.root.sapp.manager;

import java.io.File;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager.NameNotFoundException;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;

import com.snda.root.sapp.manager.adapter.AppInfo;
import com.snda.root.sapp.manager.utils.ApkUtils;
import com.snda.root.sapp.manager.utils.CommandResult;
import com.snda.root.sapp.manager.utils.RootUtils;

public class AppDetailActivity extends Activity implements OnClickListener {

	AppInfo info = null;
	PackageInfo pinfo = null;

	ImageView appIcon;
	TextView appName;
	TextView appPath;
	TextView appVersion;
	Button btnDelete;

	TextView tvPathDetail;
	TextView tvOdexDetail;
	TextView tvFileSizeDetail;
	TextView tvDataPathDetail;
	TextView tvSharedIdDetail;
	TextView tvDataSizeDetail;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.appdetail);

		appIcon = (ImageView) findViewById(R.id.appIcon);
		appName = (TextView) findViewById(R.id.appName);
		appVersion = (TextView) findViewById(R.id.appVersion);
		btnDelete = (Button) findViewById(R.id.btnDelete);

		tvPathDetail = (TextView) findViewById(R.id.tvPathDetail);
		tvOdexDetail = (TextView) findViewById(R.id.tvOdexDetail);
		tvFileSizeDetail = (TextView) findViewById(R.id.tvFileSizeDetail);
		tvDataPathDetail = (TextView) findViewById(R.id.tvDataPathDetail);
		tvSharedIdDetail = (TextView) findViewById(R.id.tvSharedIdDetail);
		tvDataSizeDetail = (TextView) findViewById(R.id.tvDataSizeDetail);

		info = GlobalInstance.currentApp;
		try {
			pinfo = GlobalInstance.pm.getPackageInfo(info.info.packageName,
					MODE_APPEND);
		} catch (NameNotFoundException e) {
			pinfo = null;
		}
		showAppInfo();

		if (!GlobalInstance.allowDeleteLevel0) {
			if (info.level == 0) {
				btnDelete.setEnabled(false);
			}
		}

		btnDelete.setOnClickListener(this);
	}

	public void showAppInfo() {
		appIcon.setBackgroundDrawable(GlobalInstance.pm
				.getApplicationIcon(info.info));
		appName.setText(GlobalInstance.pm.getApplicationLabel(info.info));
		appVersion.setText(getResources().getString(R.string.version)
				+ (pinfo == null ? getResources().getString(R.string.unknown)
						: pinfo.versionName));

		tvPathDetail.setText(info.info.sourceDir);

		String odexPath = info.info.sourceDir.substring(0, info.info.sourceDir
				.length() - 3)
				+ "odex";
		File fOdex = new File(odexPath);
		tvOdexDetail.setText(fOdex.exists() ? odexPath : getResources()
				.getString(R.string.na));

		tvFileSizeDetail.setText(ApkUtils.getAppSize(info.info.sourceDir)
				+ " KB "
				+ String.format("(%s)", fOdex.exists() ? "APK+ODEX" : "APK"));

		tvDataPathDetail.setText(info.info.dataDir);

		String dataSize = ApkUtils.getDataSize(info.info.dataDir);
		tvDataSizeDetail.setText(dataSize.equals("") ? getResources()
				.getString(R.string.unknown) : dataSize + " KB");

		String sid = pinfo.sharedUserId;
		if (sid == null) {
			sid = "";
		}
		sid = sid.trim();
		tvSharedIdDetail.setText(sid.equals("") ? getResources().getString(
				R.string.na) : sid);
	}

	@Override
	public void onClick(View arg0) {

		// 0:android|1.google|2:other
		String hintStr = "";
		switch (info.level) {
		case 0:
			hintStr = getResources().getString(R.string.delete_android_app);
			break;
		case 1:
			hintStr = getResources().getString(R.string.delete_google_app);
			break;
		case 2:
			hintStr = getResources().getString(R.string.delete_system_app);
			break;
		}

		// deleteSystemApp
		new AlertDialog.Builder(this).setTitle(R.string.hint).setMessage(
				hintStr).setPositiveButton(R.string.ok,
				new DialogInterface.OnClickListener() {

					@Override
					public void onClick(DialogInterface arg0, int arg1) {
						deleteApp();
					}
				}).setNegativeButton(R.string.cancel, null).show();

	}

	public void deleteApp() {

		String mount = RootUtils.buildMountCommand();
		if (mount.equals("")) {
			Toast.makeText(this, R.string.delete_readonly, Toast.LENGTH_LONG)
					.show();
			return;
		}
		CommandResult mt = RootUtils.runRootCommand(mount);
		if (!mt.error.equals("")) {
			Toast.makeText(this, R.string.mount_fail, Toast.LENGTH_LONG).show();
			return;
		}
		boolean ret = ApkUtils.deleteSystemApp(info.info.sourceDir);
		if (!ret) {
			Toast.makeText(this, R.string.delete_fail, Toast.LENGTH_LONG)
					.show();
			return;
		}

		Intent inRet = new Intent();
		inRet.putExtra("needRefresh", true);
		setResult(RESULT_OK, inRet);
		finish();
	}

}
