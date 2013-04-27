package com.yugioh.android.fragments;

import java.io.File;

import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.View;
import android.widget.Button;
import android.widget.ProgressBar;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.utils.DownloadUtils;
import com.rarnu.devlib.utils.FileUtils;
import com.yugioh.android.R;
import com.yugioh.android.classes.UpdateInfo;
import com.yugioh.android.database.YugiohUtils;
import com.yugioh.android.define.NetworkDefine;
import com.yugioh.android.define.PathDefine;
import com.yugioh.android.intf.IDestroyCallback;
import com.yugioh.android.intf.IUpdateIntf;

public class UpdateFragment extends BaseFragment implements IDestroyCallback,
		android.view.View.OnClickListener {

	final String dbSource = PathDefine.DOWNLOAD_PATH + PathDefine.DATA_NAME;
	final String apkSource = PathDefine.DOWNLOAD_PATH + PathDefine.APK_NAME;

	Button btnUpdateApk, btnUpdateData;
	TextView tvApkInfo, tvDataInfo;
	ProgressBar pbDownlaodingApk, pbDownlaodingData;

	UpdateInfo updateInfo = null;

	public UpdateFragment(String tagText, String tabTitle) {
		super(tagText, tabTitle);
	}

	@Override
	public int getBarTitle() {
		return R.string.page_update;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.page_update;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.fragment_update;
	}

	@Override
	protected String getMainActivityName() {
		return "";
	}

	@Override
	protected void initComponents() {
		btnUpdateApk = (Button) innerView.findViewById(R.id.btnUpdateApk);
		btnUpdateData = (Button) innerView.findViewById(R.id.btnUpdateData);
		tvApkInfo = (TextView) innerView.findViewById(R.id.tvApkInfo);
		tvDataInfo = (TextView) innerView.findViewById(R.id.tvDataInfo);
		pbDownlaodingApk = (ProgressBar) innerView
				.findViewById(R.id.pbDownlaodingApk);
		pbDownlaodingData = (ProgressBar) innerView
				.findViewById(R.id.pbDownlaodingData);
	}

	@Override
	protected void initEvents() {
		btnUpdateApk.setOnClickListener(this);
		btnUpdateData.setOnClickListener(this);
	}

	@Override
	protected void initLogic() {
		File fDownload = new File(PathDefine.DOWNLOAD_PATH);
		if (!fDownload.exists()) {
			fDownload.mkdirs();
		}
		updateInfo = (UpdateInfo) getActivity().getIntent()
				.getSerializableExtra("update");
		updateCurrentStatus();
	}

	private void updateCurrentStatus() {
		tvApkInfo.setVisibility(View.VISIBLE);
		tvDataInfo.setVisibility(View.VISIBLE);
		switch (updateInfo.getUpdateApk()) {
		case -1:
			tvApkInfo.setText(getString(R.string.update_apk_fmt,
					updateInfo.getApkVersion()));
			btnUpdateApk.setText(R.string.update_install);
			break;
		case 0:
			tvApkInfo.setText(R.string.update_no_apk);
			btnUpdateApk.setText(R.string.update_renew);
			break;
		default:
			tvApkInfo.setText(getString(R.string.update_apk_fmt,
					updateInfo.getApkVersion()));
			btnUpdateApk.setText(R.string.update_renew);
			break;

		}
		switch (updateInfo.getUpdateData()) {
		case 0:
			tvDataInfo.setText(R.string.update_no_data);
			break;
		default:
			tvDataInfo.setText(getString(R.string.update_data_fmt,
					updateInfo.getNewCard()));
			break;
		}

	}

	@Override
	protected void initMenu(Menu menu) {

	}

	@Override
	protected void onGetNewArguments(Bundle bn) {

	}

	private Handler hApkTask = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			switch (msg.what) {
			case DownloadUtils.WHAT_DOWNLOAD_START:
			case DownloadUtils.WHAT_DOWNLOAD_PROGRESS:
				pbDownlaodingApk.setMax(msg.arg2);
				pbDownlaodingApk.setProgress(msg.arg1);
				break;
			case DownloadUtils.WHAT_DOWNLOAD_FINISH:
				try {
					pbDownlaodingApk.setVisibility(View.GONE);
					((IUpdateIntf) getActivity()).setInProgress(false);
					updateInfo.setUpdateApk(-1);
					updateCurrentStatus();
					updateDisabled(true);
				} catch (Exception e) {

				}
				break;
			}
			super.handleMessage(msg);
		};
	};

	private Handler hDataTask = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			switch (msg.what) {
			case DownloadUtils.WHAT_DOWNLOAD_START:
			case DownloadUtils.WHAT_DOWNLOAD_PROGRESS:
				pbDownlaodingData.setMax(msg.arg2);
				pbDownlaodingData.setProgress(msg.arg1);
				break;
			case DownloadUtils.WHAT_DOWNLOAD_FINISH:
				try {
					YugiohUtils.closeDatabase(getActivity());
					FileUtils.copyFile(dbSource, PathDefine.DATABASE_PATH);
					FileUtils.deleteFile(dbSource);
					YugiohUtils.newDatabase(getActivity());
					pbDownlaodingData.setVisibility(View.GONE);
					((IUpdateIntf) getActivity()).setInProgress(false);
					updateInfo.setUpdateData(0);
					updateCurrentStatus();
					updateDisabled(true);
				} catch (Exception e) {

				}
				break;
			}
			super.handleMessage(msg);
		};
	};

	private void installApk() {
		File fApk = new File(apkSource);
		if (fApk.exists()) {
			Uri uri = Uri.fromFile(fApk);
			Intent intent = new Intent(Intent.ACTION_VIEW);
			intent.setDataAndType(uri,
					"application/vnd.android.package-archive");
			startActivity(intent);
		}
	}

	@Override
	public void onClick(View v) {
		((IUpdateIntf) getActivity()).setInProgress(true);
		updateDisabled(false);
		switch (v.getId()) {
		case R.id.btnUpdateApk:
			if (updateInfo.getUpdateApk() == -1) {
				installApk();
			} else {
				((IUpdateIntf) getActivity()).setUpdateFile(
						PathDefine.DOWNLOAD_PATH, PathDefine.APK_NAME);
				tvApkInfo.setVisibility(View.GONE);
				FileUtils.deleteFile(apkSource);
				pbDownlaodingApk.setVisibility(View.VISIBLE);
				DownloadUtils.downloadFileT(getActivity(), null,
						NetworkDefine.URL_APK, PathDefine.DOWNLOAD_PATH,
						PathDefine.APK_NAME, hApkTask);
			}
			break;
		case R.id.btnUpdateData:
			((IUpdateIntf) getActivity()).setUpdateFile(
					PathDefine.DOWNLOAD_PATH, PathDefine.DATA_NAME);
			tvDataInfo.setVisibility(View.GONE);
			FileUtils.deleteFile(dbSource);
			pbDownlaodingData.setVisibility(View.VISIBLE);
			DownloadUtils.downloadFileT(getActivity(), null,
					NetworkDefine.URL_DATA, PathDefine.DOWNLOAD_PATH,
					PathDefine.DATA_NAME, hDataTask);
			break;
		}
	}

	private void updateDisabled(boolean enabled) {
		if (!enabled) {
			btnUpdateApk.setEnabled(false);
			btnUpdateData.setEnabled(false);
		} else {
			if (updateInfo.getUpdateApk() != 0) {
				btnUpdateApk.setEnabled(true);
			}
			if (updateInfo.getUpdateData() != 0) {
				btnUpdateData.setEnabled(true);
			}
		}

	}

	@Override
	public void doDestroyHandler() {
		hApkTask = null;
		hDataTask = null;

	}

}
