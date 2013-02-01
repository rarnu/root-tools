package com.rarnu.tools.root.fragment;

import android.content.Intent;
import android.net.Uri;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.api.MobileApi;
import com.rarnu.tools.root.base.BaseFragment;
import com.rarnu.tools.root.comp.AlertDialogEx;
import com.rarnu.tools.root.fragmentactivity.HelpActivity;
import com.rarnu.tools.root.utils.DeviceUtils;

public class AboutFragment extends BaseFragment implements OnClickListener {

	RelativeLayout layHelp, layFitable, layUpdate;
	ImageView imgFitable;
	TextView tvAppVersion, tvDebug;
	RelativeLayout layEoe;

	int fitable = 5;
	int fitableClick = 0;

	int[] fitableDrawable = new int[] { R.drawable.c1, R.drawable.c2,
			R.drawable.c3, R.drawable.c4, R.drawable.c5, R.drawable.c6,
			R.drawable.c7, R.drawable.c8, R.drawable.c9 };

	private void showUpdateInfo() {

		if (GlobalInstance.updateInfo == null
				|| GlobalInstance.updateInfo.result == 0) {
			AlertDialogEx.showAlertDialogEx(getActivity(),
					getString(R.string.check_update),
					getString(R.string.no_update_found),
					getString(R.string.ok), null, null, null);
		} else {
			AlertDialogEx.showAlertDialogEx(getActivity(),
					getString(R.string.check_update), String.format(
							getString(R.string.update_found_info),
							GlobalInstance.updateInfo.versionName,
							GlobalInstance.updateInfo.size),
					getString(R.string.ok),
					new AlertDialogEx.DialogButtonClickListener() {

						@Override
						public void onClick(View v) {
							// download new version
							String downUrl = MobileApi.DOWNLOAD_BASE_URL
									+ GlobalInstance.updateInfo.file;
							Intent inDownload = new Intent(Intent.ACTION_VIEW);
							inDownload.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
							inDownload.setData(Uri.parse(downUrl));
							startActivity(inDownload);
						}
					}, getString(R.string.cancel), null);
		}
	}

	private void showDebugStatus() {
		tvDebug.setVisibility(GlobalInstance.DEBUG ? View.VISIBLE : View.GONE);
	}

	private void showAppVersion() {
		tvAppVersion.setText(DeviceUtils.getAppVersionName(getActivity()));
	}

	private void showSystemFitable() {

		fitable = DeviceUtils.getFitable(GlobalInstance.metric);
		if (fitable < 1) {
			fitable = 1;
		}
		if (fitable > 9) {
			fitable = 9;
		}
		imgFitable.setBackgroundResource(fitableDrawable[fitable - 1]);
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.layUpdate:
			showUpdateInfo();
			break;
		case R.id.layHelp:
			GlobalFragment.showContent(getActivity(), new Intent(getActivity(),
					HelpActivity.class), GlobalFragment.fIntro);
			break;
		case R.id.layEoe:
			// goto eoemarket
			Intent inEoe = new Intent(Intent.ACTION_VIEW);
			inEoe.setData(Uri.parse("http://eoemarket.com/"));
			startActivity(inEoe);
			break;
		}

	}

	@Override
	protected int getBarTitle() {
		return R.string.about;
	}

	@Override
	protected int getBarTitleWithPath() {
		return R.string.about_with_path;
	}

	@Override
	protected void initComponents() {
		layHelp = (RelativeLayout) innerView.findViewById(R.id.layHelp);
		layFitable = (RelativeLayout) innerView.findViewById(R.id.layFitable);
		imgFitable = (ImageView) innerView.findViewById(R.id.imgFitable);
		tvAppVersion = (TextView) innerView.findViewById(R.id.tvAppVersion);
		tvDebug = (TextView) innerView.findViewById(R.id.tvDebug);
		layUpdate = (RelativeLayout) innerView.findViewById(R.id.layUpdate);
		layEoe = (RelativeLayout) innerView.findViewById(R.id.layEoe);

		layUpdate.setOnClickListener(this);
		layHelp.setOnClickListener(this);
		layEoe.setOnClickListener(this);

	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.layout_about;
	}

	@Override
	protected void initMenu(Menu menu) {

	}

	@Override
	protected void initLogic() {
		showAppVersion();
		showSystemFitable();
		showDebugStatus();
		LogApi.logEnterAbout();
		fitableClick = 0;
	}

}
