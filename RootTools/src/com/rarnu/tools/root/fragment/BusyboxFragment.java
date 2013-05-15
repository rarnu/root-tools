package com.rarnu.tools.root.fragment;

import java.util.ArrayList;
import java.util.List;

import android.app.AlertDialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;

import com.rarnu.command.RootUtils;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.BlockListView;
import com.rarnu.devlib.component.DataProgressBar;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.BusyboxAdapter;
import com.rarnu.tools.root.common.BusyboxInfo;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.utils.BusyboxUtils;
import com.rarnu.utils.UIUtils;

public class BusyboxFragment extends BaseFragment implements
		OnItemClickListener {

	DataProgressBar progressBusybox;
	BlockListView lstBusybox;
	List<BusyboxInfo> list = null;
	BusyboxAdapter adapter = null;
	MenuItem itemHelp;

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MenuItemIds.MENU_HELP:
			showHelp();
			break;
		}
		return true;
	}

	private Handler hReinstallBusybox = new Handler() {

		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {
				progressBusybox.setVisibility(View.GONE);
				checkStatus();
			}
			super.handleMessage(msg);
		}

	};

	private void doReinstallBusyboxT() {
		progressBusybox.setAppName(getString(R.string.installing));
		progressBusybox.setVisibility(View.VISIBLE);
		new Thread(new Runnable() {
			@Override
			public void run() {
				BusyboxUtils.removeBusybox();
				BusyboxUtils.installBusybox(getActivity());
				hReinstallBusybox.sendEmptyMessage(1);
			}
		}).start();
	}

	private void reinstallBusybox() {
		int ret = RootUtils.hasRoot();
		if (ret == 0) {
			showSuStatus();
			return;
		}

		new AlertDialog.Builder(getActivity())
				.setTitle(R.string.hint)
				.setMessage(
						RootUtils.hasBusybox() ? R.string.confirm_reinstall_busybox
								: R.string.confirm_install_busybox)
				.setPositiveButton(R.string.ok,
						new DialogInterface.OnClickListener() {

							@Override
							public void onClick(DialogInterface dialog,
									int which) {
								doReinstallBusyboxT();

							}
						}).setNegativeButton(R.string.cancel, null).show();

	}

	private void checkStatus() {
		boolean hasSu = RootUtils.hasSu();
		boolean isWrong = RootUtils.isWrongRoot();

		list.clear();
		list.add(buildBusyboxInfo(R.string.file_su,
				(hasSu ? (isWrong ? BusyboxInfo.STATE_WARNING
						: BusyboxInfo.STATE_NORMAL) : BusyboxInfo.STATE_BANNED)));
		list.add(buildBusyboxInfo(R.string.file_super_user, (RootUtils
				.hasSuperuser() ? BusyboxInfo.STATE_NORMAL
				: BusyboxInfo.STATE_WARNING)));
		list.add(buildBusyboxInfo(R.string.file_busybox, (RootUtils
				.hasBusybox() ? BusyboxInfo.STATE_NORMAL
				: BusyboxInfo.STATE_WARNING)));

		adapter.setNewList(list);
		lstBusybox.resize();
	}

	private BusyboxInfo buildBusyboxInfo(int resTitle, int state) {
		BusyboxInfo info = new BusyboxInfo();
		info.title = getString(resTitle);
		info.state = state;
		return info;
	}

	private void showHelp() {
		new AlertDialog.Builder(getActivity()).setTitle(R.string.help)
				.setMessage(R.string.help_busybox)
				.setPositiveButton(R.string.ok, null).show();
	}

	private void showSuStatus() {
		int ret = RootUtils.hasRoot();
		if (RootUtils.isWrongRoot()) {
			ret = 0;
		}
		new AlertDialog.Builder(getActivity())
				.setTitle(R.string.hint)
				.setMessage(
						(ret == 0 ? R.string.no_root_permission
								: R.string.has_su_file))
				.setPositiveButton(R.string.ok, null).show();

	}

	@Override
	public int getBarTitle() {
		return R.string.busybox;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.busybox_with_path;
	}

	@Override
	protected void initComponents() {
		lstBusybox = (BlockListView) innerView.findViewById(R.id.lstBusybox);
		progressBusybox = (DataProgressBar) innerView
				.findViewById(R.id.progressBusybox);
		lstBusybox.setItemHeight(UIUtils.dipToPx(56));

		list = new ArrayList<BusyboxInfo>();
		adapter = new BusyboxAdapter(getActivity(), list);
		lstBusybox.setAdapter(adapter);

	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.layout_busybox;
	}

	@Override
	protected void initMenu(Menu menu) {
		itemHelp = menu.add(0, MenuItemIds.MENU_HELP, 99, R.string.help);
		itemHelp.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
		itemHelp.setIcon(android.R.drawable.ic_menu_help);
	}

	@Override
	protected void initLogic() {
		checkStatus();
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		switch (position) {
		case 0:
			showSuStatus();
			break;
		case 2:
			reinstallBusybox();
			break;
		}
	}

	@Override
	protected void initEvents() {
		lstBusybox.setOnItemClickListener(this);
	}

	@Override
	protected String getMainActivityName() {
		return MainActivity.class.getName();
	}

	@Override
	protected void onGetNewArguments(Bundle bn) {

	}

	@Override
	public String getCustomTitle() {
		return null;
	}

}
