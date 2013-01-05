package com.rarnu.tools.root.fragment;

import java.util.ArrayList;
import java.util.List;

import android.app.Activity;
import android.content.Intent;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.BaseAdapter;
import android.widget.ListView;
import android.widget.SearchView;
import android.widget.SearchView.OnQueryTextListener;
import android.widget.Toast;

import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.HostsAdapter;
import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.base.BaseFragment;
import com.rarnu.tools.root.common.HostRecordInfo;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.common.RTConsts;
import com.rarnu.tools.root.comp.DataBar;
import com.rarnu.tools.root.comp.DataProgressBar;
import com.rarnu.tools.root.fragmentactivity.HostAddActivity;
import com.rarnu.tools.root.loader.HostsLoader;
import com.rarnu.tools.root.utils.DIPairUtils;

public class HostFragment extends BaseFragment implements OnClickListener,
		OnLoadCompleteListener<List<HostRecordInfo>>, OnQueryTextListener {

	ListView lvHosts;
	DataBar barHosts;
	DataProgressBar progressHosts;

	List<HostRecordInfo> listHostsAll = new ArrayList<HostRecordInfo>();
	HostsAdapter hostsAdapter = null;

	HostsLoader loader = null;

	Handler hSelectHost = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {
				showHostSelectedCount();
			}
			super.handleMessage(msg);
		};

	};

	@Override
	protected int getBarTitle() {
		return R.string.func7_title;
	}

	@Override
	protected int getBarTitleWithPath() {
		return R.string.func7_title_with_path;
	}

	@Override
	protected void initComponents() {
		barHosts = (DataBar) innerView.findViewById(R.id.barHosts);
		progressHosts = (DataProgressBar) innerView
				.findViewById(R.id.progressHosts);
		lvHosts = (ListView) innerView.findViewById(R.id.lvHosts);

		barHosts.getButton1().setOnClickListener(this);
		barHosts.getButton2().setOnClickListener(this);
		barHosts.getCheckBox().setOnClickListener(this);

		hostsAdapter = new HostsAdapter(getActivity().getLayoutInflater(),
				listHostsAll, hSelectHost, true, true);
		lvHosts.setAdapter(hostsAdapter);
		loader = new HostsLoader(getActivity());
		loader.registerListener(0, this);

	}

	@Override
	protected void initLogic() {
		doStartLoad();

	}

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		LogApi.logEnterHosts();
	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.layout_hosts;
	}

	@Override
	protected void initMenu(Menu menu) {
		MenuItem itemSearch = menu.add(0, MenuItemIds.MENU_SEARCH, 98,
				R.string.search);
		itemSearch.setIcon(android.R.drawable.ic_menu_search);
		itemSearch.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
		SearchView sv = new SearchView(getActivity());
		sv.setOnQueryTextListener(this);
		itemSearch.setActionView(sv);

		MenuItem itemAdd = menu.add(0, MenuItemIds.MENU_ADD, 99, R.string.add);
		itemAdd.setIcon(android.R.drawable.ic_menu_add);
		itemAdd.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);

		MenuItem itemRefresh = menu.add(0, MenuItemIds.MENU_REFRESH, 100,
				R.string.refresh);
		itemRefresh.setIcon(android.R.drawable.ic_menu_revert);
		itemRefresh.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);

	}
	
	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MenuItemIds.MENU_REFRESH:
			doStartLoad();
			break;
		case MenuItemIds.MENU_ADD:
			Intent inHost = new Intent(getActivity(), HostAddActivity.class);
			startActivityForResult(inHost, RTConsts.REQCODE_HOST);
			break;
		}
		return true;
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.barButton1:
			doDeleteHosts();
			break;
		case R.id.barButton2:
			setHostItemSelectedStatus(listHostsAll, hostsAdapter, hSelectHost,
					false);
			break;
		case R.id.chkSelAll:
			boolean selected = barHosts.getCheckBox().isChecked();
			setHostItemSelectedStatus(listHostsAll, hostsAdapter, hSelectHost,
					selected);
			break;
		}

	}

	private void doStartLoad() {
		barHosts.setVisibility(View.GONE);
		progressHosts.setAppName(getString(R.string.loading));
		progressHosts.setVisibility(View.VISIBLE);
		loader.startLoading();
	}

	@Override
	public void onLoadComplete(Loader<List<HostRecordInfo>> loader,
			List<HostRecordInfo> data) {

		listHostsAll.clear();
		if (data != null) {
			listHostsAll.addAll(data);
		}

		hostsAdapter.setNewData(listHostsAll);
		progressHosts.setVisibility(View.GONE);
		showHostSelectedCount();

	}

	private void showHostSelectedCount() {
		try {
			int count = getHostSelectedCount(listHostsAll);
			String cap = String.format(
					getResources().getString(R.string.btn_delete), count);
			barHosts.setButton1Text(cap);
			barHosts.setVisibility(count == 0 ? View.GONE : View.VISIBLE);
		} catch (Exception e) {

		}

	}

	private int getHostSelectedCount(List<HostRecordInfo> list) {
		int count = 0;
		if (list != null) {
			for (int i = 0; i < list.size(); i++) {
				if (list.get(i).checked) {
					count++;
				}
			}
		}
		return count;
	}

	private void setHostItemSelectedStatus(List<HostRecordInfo> list,
			BaseAdapter adapter, Handler h, boolean selected) {
		for (int i = 0; i < list.size(); i++) {
			list.get(i).checked = selected;
		}
		adapter.notifyDataSetChanged();
		h.sendEmptyMessage(1);
	}

	private void doDeleteHosts() {
		// delete hosts
		lvHosts.setEnabled(false);
		barHosts.setVisibility(View.GONE);
		progressHosts.setAppName(getString(R.string.deleting));
		progressHosts.setVisibility(View.VISIBLE);

		LogApi.logDeleteHosts();

		final List<HostRecordInfo> deletedHosts = new ArrayList<HostRecordInfo>();

		final Handler h = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					if (msg.arg1 == 0) {
						doStartLoad();
						Toast.makeText(getActivity(),
								R.string.save_hosts_error, Toast.LENGTH_LONG)
								.show();
					}
					lvHosts.setEnabled(true);
					hostsAdapter.deleteItem(deletedHosts);

					progressHosts.setVisibility(View.GONE);

				}
				super.handleMessage(msg);
			}

		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				int count = listHostsAll.size();
				for (int i = count - 1; i >= 0; i--) {
					if (listHostsAll.get(i).checked) {
						deletedHosts.add(listHostsAll.get(i));
						listHostsAll.remove(i);
					}
				}
				boolean ret = DIPairUtils.saveHosts(listHostsAll);
				Message msg = new Message();
				msg.what = 1;
				msg.arg1 = (ret ? 1 : 0);
				h.sendMessage(msg);

			}
		}).start();
	}

	private void doMergeHosts(final String[] hosts) {
		if (hosts == null || hosts.length == 0) {
			return;
		}

		lvHosts.setEnabled(false);
		barHosts.setVisibility(View.GONE);
		progressHosts.setAppName(getString(R.string.adding));
		progressHosts.setVisibility(View.VISIBLE);

		LogApi.logAddHosts();

		final Handler h = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					if (msg.arg1 == 0) {
						doStartLoad();
						Toast.makeText(getActivity(),
								R.string.save_hosts_error, Toast.LENGTH_LONG)
								.show();
					}
					lvHosts.setEnabled(true);
					hostsAdapter.notifyDataSetChanged();
					progressHosts.setVisibility(View.GONE);

				}
				super.handleMessage(msg);
			}

		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				DIPairUtils.mergeHosts(listHostsAll, hosts);
				boolean ret = DIPairUtils.saveHosts(listHostsAll);
				Message msg = new Message();
				msg.what = 1;
				msg.arg1 = (ret ? 1 : 0);
				h.sendMessage(msg);

			}
		}).start();
	}

	@Override
	public void onActivityResult(int requestCode, int resultCode, Intent data) {
		if (resultCode != Activity.RESULT_OK) {
			return;
		}
		switch (requestCode) {

		case RTConsts.REQCODE_HOST:
			doMergeHosts(data.getStringArrayExtra("hosts"));
			break;

		}
	}

	@Override
	public boolean onQueryTextSubmit(String query) {

		return false;
	}

	@Override
	public boolean onQueryTextChange(String newText) {
		if (hostsAdapter != null) {
			hostsAdapter.getFilter().filter(newText);
		}
		return true;
	}

}
