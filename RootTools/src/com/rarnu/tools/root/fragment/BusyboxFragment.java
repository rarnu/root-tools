package com.rarnu.tools.root.fragment;

import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import com.rarnu.tools.root.R;
import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.base.BaseFragment;
import com.rarnu.tools.root.base.MenuItemIds;
import com.rarnu.tools.root.comp.AlertDialogEx;
import com.rarnu.tools.root.comp.DataProgressBar;
import com.rarnu.tools.root.utils.BusyboxUtils;
import com.rarnu.tools.root.utils.DeviceUtils;
import com.rarnu.tools.root.utils.root.RootUtils;

public class BusyboxFragment extends BaseFragment implements OnClickListener {

	RelativeLayout laySu, laySuperuser, layBusybox;
	ImageView imgSu, imgSuperuser, imgBusybox;
	DataProgressBar progressBusybox;

	@Override
	public void onResume() {
		super.onResume();

	}

	@Override
	public void onActivityCreated(Bundle savedInstanceState) {
		super.onActivityCreated(savedInstanceState);
		checkStatus();
		LogApi.logEnterRootBusybox();
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MenuItemIds.MENU_HELP:
			showHelp();
			break;
		}
		return true;
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
				BusyboxUtils.installBusybox(getActivity());
				h.sendEmptyMessage(1);
			}
		}).start();
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
				BusyboxUtils.installSuperuser(getActivity(), isICS);
				h.sendEmptyMessage(1);
			}
		}).start();

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
		AlertDialogEx.showAlertDialogEx(getActivity(),
				getString(R.string.hint),
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

	private void showXiaomiHint(final boolean isIcs) {
		if (isIcs) {
			AlertDialogEx.showAlertDialogEx(getActivity(),
					getString(R.string.hint), getString(R.string.hint_xiaomi),
					getString(R.string.ok),
					new AlertDialogEx.DialogButtonClickListener() {

						@Override
						public void onClick(View v) {
							doReinstallSuperuserT(isIcs);

						}
					}, getString(R.string.cancel), null);
		} else {
			AlertDialogEx.showAlertDialogEx(getActivity(),
					getString(R.string.hint), getString(R.string.hint_xiaomi),
					getString(R.string.ok), null, null, null);
		}
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

	private void showHelp() {
		// help
		AlertDialogEx.showAlertDialogEx(getActivity(),
				getString(R.string.help), getString(R.string.help_busybox),
				getString(R.string.ok), null, null, null);
	}

	private void showSuStatus() {
		int ret = RootUtils.hasRoot();
		if (RootUtils.isWrongRoot()) {
			ret = 0;
		}
		AlertDialogEx.showAlertDialogEx(getActivity(),
				getString(R.string.hint),
				getString(ret == 0 ? R.string.no_root_permission
						: R.string.has_su_file), getString(R.string.ok), null,
				null, null);
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
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

	@Override
	protected int getBarTitle() {
		return R.string.busybox;
	}

	@Override
	protected int getBarTitleWithPath() {
		return R.string.busybox_with_path;
	}

	@Override
	protected void initComponents() {
		laySu = (RelativeLayout) innerView.findViewById(R.id.laySu);
		laySuperuser = (RelativeLayout) innerView
				.findViewById(R.id.laySuperuser);
		layBusybox = (RelativeLayout) innerView.findViewById(R.id.layBusybox);
		imgSu = (ImageView) innerView.findViewById(R.id.imgSu);
		imgSuperuser = (ImageView) innerView.findViewById(R.id.imgSuperUser);
		imgBusybox = (ImageView) innerView.findViewById(R.id.imgBusybox);
		progressBusybox = (DataProgressBar) innerView
				.findViewById(R.id.progressBusybox);

		laySu.setOnClickListener(this);
		laySuperuser.setOnClickListener(this);
		layBusybox.setOnClickListener(this);

	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.layout_busybox;
	}

	@Override
	protected void initMenu(Menu menu) {
		MenuItem itemHelp = menu.add(0, MenuItemIds.MENU_HELP, 99,
				R.string.help);
		itemHelp.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
		itemHelp.setIcon(android.R.drawable.ic_menu_help);
		
	}


}
