package com.rarnu.tools.root.fragment;

import java.io.File;

import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager.NameNotFoundException;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.base.BasePopupFragment;
import com.rarnu.tools.root.common.Actions;
import com.rarnu.tools.root.common.SysappInfo;
import com.rarnu.tools.root.comp.AlertDialogEx;
import com.rarnu.tools.root.utils.ApkUtils;

public class SysappDetailFragment extends BasePopupFragment implements
		OnClickListener {

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

	// [/region]

	// [region] variable define
	SysappInfo info = null;
	PackageInfo pinfo = null;

	@Override
	public void onActivityCreated(Bundle savedInstanceState) {
		super.onActivityCreated(savedInstanceState);
		showAppInfo();
	}

	@Override
	protected int getBarTitle() {
		return R.string.sysapp_name;
	}

	@Override
	protected int getBarTitleWithPath() {
		return R.string.sysapp_name;
	}

	public void showAppInfo() {
		info = GlobalInstance.currentSysapp;
		try {
			pinfo = GlobalInstance.pm.getPackageInfo(info.info.packageName,
					Context.MODE_APPEND);
		} catch (NameNotFoundException e) {
			pinfo = null;
		}
		
		if (pinfo == null) {
			getActivity().finish();
			return;
		}

		appIcon.setBackgroundDrawable(GlobalInstance.pm
				.getApplicationIcon(info.info));
		appName.setText(GlobalInstance.pm.getApplicationLabel(info.info));
		appVersion.setText(getResources().getString(R.string.version)
				+ (pinfo == null ? getResources().getString(R.string.unknown)
						: pinfo.versionName));

		tvPathDetail.setText(info.info.sourceDir.replace("/system/app/", ""));

		String odexPath = info.info.sourceDir.substring(0,
				info.info.sourceDir.length() - 3)
				+ "odex";
		File fOdex = new File(odexPath);
		tvOdexDetail.setText(fOdex.exists() ? odexPath.replace("/system/app/",
				"") : getResources().getString(R.string.na));

		tvFileSizeDetail.setText(ApkUtils.getAppSize(info.info.sourceDir)
				+ " KB "
				+ String.format("(%s)", fOdex.exists() ? "APK+ODEX" : "APK"));

		tvDataPathDetail.setText(info.info.dataDir.replace("/data/data/", ""));

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
		if (!GlobalInstance.allowDeleteLevel0) {
			if (info.level == 0) {
				RelativeLayout.LayoutParams rlp = (RelativeLayout.LayoutParams) btnDelete
						.getLayoutParams();
				rlp.height = 0;
				btnDelete.setLayoutParams(rlp);
				btnDelete.setEnabled(false);
			}
		}
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnDelete:
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
				hintStr = getResources().getString(R.string.delete_htc_app);
				break;
			case 3:
				hintStr = getResources().getString(R.string.delete_system_app);
				break;
			}

			// delete system app
			AlertDialogEx.showAlertDialogEx(getActivity(),
					getString(R.string.hint), hintStr, getString(R.string.ok),
					new AlertDialogEx.DialogButtonClickListener() {

						@Override
						public void onClick(View v) {
							Intent inUninstall = new Intent(Actions.ACTION_UNINSTALL_APK);
							inUninstall.putExtra("backup", GlobalInstance.backupBeforeDelete);
							inUninstall.putExtra("deleteData", GlobalInstance.alsoDeleteData);
							inUninstall.putExtra("info", info.info);
							getActivity().finish();
							getActivity().sendBroadcast(inUninstall);
						}
					}, getString(R.string.cancel), null);
			break;
		}

	}

	@Override
	protected void initComponents() {
		appIcon = (ImageView) innerView.findViewById(R.id.appIcon);
		appName = (TextView) innerView.findViewById(R.id.appName);
		appVersion = (TextView) innerView.findViewById(R.id.appVersion);
		btnDelete = (Button) innerView.findViewById(R.id.btnDelete);

		tvPathDetail = (TextView) innerView.findViewById(R.id.tvPathDetail);
		tvOdexDetail = (TextView) innerView.findViewById(R.id.tvOdexDetail);
		tvFileSizeDetail = (TextView) innerView
				.findViewById(R.id.tvFileSizeDetail);
		tvDataPathDetail = (TextView) innerView
				.findViewById(R.id.tvDataPathDetail);
		tvSharedIdDetail = (TextView) innerView
				.findViewById(R.id.tvSharedIdDetail);
		tvDataSizeDetail = (TextView) innerView
				.findViewById(R.id.tvDataSizeDetail);

		btnDelete.setOnClickListener(this);

	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.layout_sysapp_detail;
	}

}
