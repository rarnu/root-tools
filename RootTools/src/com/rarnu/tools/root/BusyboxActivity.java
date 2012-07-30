package com.rarnu.tools.root;

import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.base.BaseActivity;
import com.rarnu.tools.root.comp.AlertDialogEx;
import com.rarnu.tools.root.comp.DataProgressBar;
import com.rarnu.tools.root.utils.BusyboxUtils;
import com.rarnu.tools.root.utils.DeviceUtils;
import com.rarnu.tools.root.utils.root.RootUtils;

public class BusyboxActivity extends BaseActivity implements OnClickListener {

	// [region] field define
	RelativeLayout laySu, laySuperuser, layBusybox;
	ImageView imgSu, imgSuperuser, imgBusybox;
	DataProgressBar progressBusybox;

	// [/region]

	// [region] life circle
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.layout_busybox);
		init();
		checkStatus();
		setResult(RESULT_OK);
		LogApi.logEnterRootBusybox();
	}

	// [/region]

	// [region] events
	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnLeft:
			finish();
			break;
		case R.id.btnRight:
			showHelp();
			break;
		case R.id.laySu:
			showSuStatus();
			break;
		case R.id.laySuperuser:
			reinstallSuperuser();
			break;
		case R.id.layBusybox:
			reinstallBusybox();
			break;
		}
	}

	// [/region]

	// [region] business logic
	private void showHelp() {
		// help
		AlertDialogEx.showAlertDialogEx(this, getString(R.string.help),
				getString(R.string.help_busybox), getString(R.string.ok), null,
				null, null);
	}

	private void showSuStatus() {
		int ret = RootUtils.hasRoot();
		if (RootUtils.isWrongRoot()) {
			ret = 0;
		}
		AlertDialogEx.showAlertDialogEx(this, getString(R.string.hint),
				getString(ret == 0 ? R.string.no_root_permission
						: R.string.has_su_file), getString(R.string.ok), null,
				null, null);
	}

	private void checkStatus() {
		boolean hasSu = RootUtils.hasSu();
		boolean isWrong = RootUtils.isWrongRoot();
		if (!hasSu) {
			imgSu.setBackgroundResource(R.drawable.banned);
		} else if (hasSu && isWrong) {
			imgSu.setBackgroundResource(R.drawable.warning);
		} else {
			imgSu.setBackgroundResource(R.drawable.ok);
		}

		imgSuperuser
				.setBackgroundResource(RootUtils.hasSuperuser() ? R.drawable.ok
						: R.drawable.warning);
		imgBusybox.setBackgroundResource(RootUtils.hasBusybox() ? R.drawable.ok
				: R.drawable.warning);
	}

	private void reinstallSuperuser() {

		int ret = RootUtils.hasRoot();
		if (ret == 0) {
			showSuStatus();
			return;
		}

		if (!RootUtils.hasSuperuser()) {

			int sysVersionCode = android.os.Build.VERSION.SDK_INT;

			if (DeviceUtils.getBuildProp(DeviceUtils.RO_PRODUCT_MANUFACTURER)
					.toLowerCase().contains("xiaomi")) {
				showXiaomiHint(sysVersionCode >= 11);
				return;
			}

			if (DeviceUtils.getBuildProp(DeviceUtils.RO_BUILD_ID).toLowerCase()
					.contains("miui")) {
				showXiaomiHint(sysVersionCode >= 11);
				return;
			}

			doReinstallSuperuserT(sysVersionCode >= 14);
			return;
		}
	}

	private void showXiaomiHint(final boolean isIcs) {
		if (isIcs) {
			AlertDialogEx.showAlertDialogEx(this, getString(R.string.hint),
					getString(R.string.hint_xiaomi), getString(R.string.ok),
					new AlertDialogEx.DialogButtonClickListener() {

						@Override
						public void onClick(View v) {
							doReinstallSuperuserT(isIcs);

						}
					}, getString(R.string.cancel), null);
		} else {
			AlertDialogEx.showAlertDialogEx(this, getString(R.string.hint),
					getString(R.string.hint_xiaomi), getString(R.string.ok),
					null, null, null);
		}
	}

	private void reinstallBusybox() {

		int ret = RootUtils.hasRoot();
		if (ret == 0) {
			showSuStatus();
			return;
		}

		if (!RootUtils.hasBusybox()) {
			doReinstallBusyboxT();
			return;
		}

		// reinstall busybox
		AlertDialogEx.showAlertDialogEx(this, getString(R.string.hint),
				getString(R.string.confirm_reinstall_busybox),
				getString(R.string.ok),
				new AlertDialogEx.DialogButtonClickListener() {

					@Override
					public void onClick(View v) {
						doReinstallBusyboxT();
						LogApi.logReinstallBusybox();
					}
				}, getString(R.string.cancel), null);
	}

	private void doReinstallSuperuserT(final boolean isICS) {
		// do reinstall superuser
		progressBusybox.setAppName(getString(R.string.installing));
		progressBusybox.setVisibility(View.VISIBLE);
		final Handler h = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					progressBusybox.setVisibility(View.GONE);
					checkStatus();
				}
				super.handleMessage(msg);
			}

		};
		new Thread(new Runnable() {
			@Override
			public void run() {
				BusyboxUtils.removeSuperuser();
				BusyboxUtils.installSuperuser(BusyboxActivity.this, isICS);
				h.sendEmptyMessage(1);
			}
		}).start();

	}

	private void doReinstallBusyboxT() {
		progressBusybox.setAppName(getString(R.string.installing));
		progressBusybox.setVisibility(View.VISIBLE);
		final Handler h = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					progressBusybox.setVisibility(View.GONE);
					checkStatus();
				}
				super.handleMessage(msg);
			}

		};
		new Thread(new Runnable() {

			@Override
			public void run() {
				BusyboxUtils.removeBusybox();
				BusyboxUtils.installBusybox(BusyboxActivity.this);
				h.sendEmptyMessage(1);
			}
		}).start();
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

		laySu = (RelativeLayout) findViewById(R.id.laySu);
		laySuperuser = (RelativeLayout) findViewById(R.id.laySuperuser);
		layBusybox = (RelativeLayout) findViewById(R.id.layBusybox);
		imgSu = (ImageView) findViewById(R.id.imgSu);
		imgSuperuser = (ImageView) findViewById(R.id.imgSuperUser);
		imgBusybox = (ImageView) findViewById(R.id.imgBusybox);
		progressBusybox = (DataProgressBar) findViewById(R.id.progressBusybox);
	}

	@Override
	public void initTitle() {
		tvName.setText(R.string.busybox);
		btnLeft.setText(R.string.back);
		btnLeft.setVisibility(View.VISIBLE);
		btnRight.setText(R.string.help);
		btnRight.setVisibility(View.VISIBLE);

		// tbTitle.setText(getString(R.string.busybox));
		// tbTitle.setLeftButtonText(getString(R.string.back));
		// tbTitle.setRightButtonText(getString(R.string.help));
		// tbTitle.getLeftButton().setVisibility(View.VISIBLE);
		// tbTitle.getRightButton().setVisibility(View.VISIBLE);
	}

	@Override
	public void initSearchBar() {

	}

	@Override
	public void initEvents() {
		laySu.setOnClickListener(this);
		laySuperuser.setOnClickListener(this);
		layBusybox.setOnClickListener(this);
		btnLeft.setOnClickListener(this);
		btnRight.setOnClickListener(this);
		// tbTitle.getLeftButton().setOnClickListener(this);
		// tbTitle.getRightButton().setOnClickListener(this);
	}

	// [/region]
}
