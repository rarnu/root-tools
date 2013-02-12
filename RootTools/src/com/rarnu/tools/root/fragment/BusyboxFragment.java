package com.rarnu.tools.root.fragment;

import java.util.ArrayList;
import java.util.List;

import android.app.AlertDialog;
import android.content.DialogInterface;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;

import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.BusyboxAdapter;
import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.base.BaseFragment;
import com.rarnu.tools.root.common.BusyboxInfo;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.comp.BlockListView;
import com.rarnu.tools.root.comp.DataProgressBar;
import com.rarnu.tools.root.utils.BusyboxUtils;
import com.rarnu.tools.root.utils.UIUtils;
import com.rarnu.tools.root.utils.root.RootUtils;

public class BusyboxFragment extends BaseFragment implements
		OnItemClickListener {

	DataProgressBar progressBusybox;
	BlockListView lstBusybox;
	List<BusyboxInfo> list = null;
	BusyboxAdapter adapter = null;

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
		new AlertDialog.Builder(getActivity())
				.setTitle(R.string.hint)
				.setMessage(R.string.confirm_reinstall_busybox)
				.setPositiveButton(R.string.ok,
						new DialogInterface.OnClickListener() {

							@Override
							public void onClick(DialogInterface dialog,
									int which) {
								doReinstallBusyboxT();
								LogApi.logReinstallBusybox();

							}
						}).setNegativeButton(R.string.cancel, null).show();

	}

	private void checkStatus() {
		boolean hasSu = RootUtils.hasSu();
		boolean isWrong = RootUtils.isWrongRoot();

		list.clear();

		BusyboxInfo infoSu = new BusyboxInfo();
		infoSu.title = getString(R.string.file_su);
		infoSu.state = (hasSu ? (isWrong ? BusyboxInfo.STATE_WARNING
				: BusyboxInfo.STATE_NORMAL) : BusyboxInfo.STATE_BANNED);
		list.add(infoSu);

		BusyboxInfo infoSuperUser = new BusyboxInfo();
		infoSuperUser.title = getString(R.string.file_super_user);
		infoSuperUser.state = (RootUtils.hasSuperuser() ? BusyboxInfo.STATE_NORMAL
				: BusyboxInfo.STATE_WARNING);
		list.add(infoSuperUser);

		BusyboxInfo infoBusybox = new BusyboxInfo();
		infoBusybox.title = getString(R.string.file_busybox);
		infoBusybox.state = (RootUtils.hasBusybox() ? BusyboxInfo.STATE_NORMAL
				: BusyboxInfo.STATE_WARNING);
		list.add(infoBusybox);

		adapter.setNewList(list);
		lstBusybox.resize();
	}

	private void showHelp() {
		// help
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
	protected int getBarTitle() {
		return R.string.busybox;
	}

	@Override
	protected int getBarTitleWithPath() {
		return R.string.busybox_with_path;
	}

	@Override
	protected void initComponents() {
		lstBusybox = (BlockListView) innerView.findViewById(R.id.lstBusybox);
		progressBusybox = (DataProgressBar) innerView
				.findViewById(R.id.progressBusybox);
		lstBusybox.setItemHeight(UIUtils.dipToPx(56));
		lstBusybox.setOnItemClickListener(this);
		
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
		MenuItem itemHelp = menu.add(0, MenuItemIds.MENU_HELP, 99,
				R.string.help);
		itemHelp.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
		itemHelp.setIcon(android.R.drawable.ic_menu_help);

	}

	@Override
	protected void initLogic() {
		checkStatus();
		LogApi.logEnterRootBusybox();
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

}
