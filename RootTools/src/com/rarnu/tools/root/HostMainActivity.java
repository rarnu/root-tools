package com.rarnu.tools.root;

import java.util.ArrayList;
import java.util.List;

import android.app.Fragment;
import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.BaseAdapter;
import android.widget.ListView;
import android.widget.Toast;

import com.rarnu.tools.root.adapter.HostsAdapter;
import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.base.BaseActivity;
import com.rarnu.tools.root.common.HostRecordInfo;
import com.rarnu.tools.root.common.RTConsts;
import com.rarnu.tools.root.comp.DataBar;
import com.rarnu.tools.root.comp.DataProgressBar;
import com.rarnu.tools.root.comp.SearchBar;
import com.rarnu.tools.root.utils.DIPairUtils;


public class HostMainActivity extends BaseActivity implements OnClickListener {

	// [region] field define

	ListView lvHosts;
	SearchBar sbHosts;
	DataBar barHosts;
	DataProgressBar progressHosts;
	// [/region]

	// [region] variable define
	List<HostRecordInfo> listHostsAll = new ArrayList<HostRecordInfo>();
	HostsAdapter hostsAdapter = null;
	// [/region]

	// [region] handler define
	Handler hSelectHost = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {
				showHostSelectedCount();
			}
			super.handleMessage(msg);
		};

	};

	// [/region]

	// [region] life circle
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.layout_hosts);

		loadHosts();
		LogApi.logEnterHosts();
	}

	// [/region]

	// [region] init

	
	
	public void mappingComp() {

		barHosts = (DataBar) findViewById(R.id.barHosts);
		progressHosts = (DataProgressBar) findViewById(R.id.progressHosts);
		sbHosts = (SearchBar) findViewById(R.id.sbHosts);
		lvHosts = (ListView) findViewById(R.id.lvHosts);
	}

	

	
	public void initEvents() {
		sbHosts.getCancelButton().setOnClickListener(this);
		sbHosts.getAddButton().setOnClickListener(this);
		sbHosts.getEditText().addTextChangedListener(new TextWatcher() {

			@Override
			public void onTextChanged(CharSequence s, int start, int before,
					int count) {

			}

			@Override
			public void beforeTextChanged(CharSequence s, int start, int count,
					int after) {

			}

			@Override
			public void afterTextChanged(Editable s) {
				if (s == null || hostsAdapter == null) {
					return;
				}
				hostsAdapter.getFilter().filter(sbHosts.getText().toString());
			}
		});

		barHosts.getButton1().setOnClickListener(this);
		barHosts.getButton2().setOnClickListener(this);
		

		barHosts.getCheckBox().setOnClickListener(this);
	}

	// [/region]

	// [region] business logic
	private void loadHosts() {

		barHosts.setVisibility(View.GONE);
		progressHosts.setAppName(getString(R.string.loading));
		progressHosts.setVisibility(View.VISIBLE);
		

		final Handler h = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					if (listHostsAll != null) {
						hostsAdapter = new HostsAdapter(getLayoutInflater(),
								listHostsAll, hSelectHost, true, true);
					} else {
						hostsAdapter = null;
					}
					lvHosts.setAdapter(hostsAdapter);
					progressHosts.setVisibility(View.GONE);
					showHostSelectedCount();
					
				}
				super.handleMessage(msg);
			}

		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				listHostsAll = DIPairUtils.getHostList();
				h.sendEmptyMessage(1);

			}
		}).start();
	}

	private void showHostSelectedCount() {
		int count = getHostSelectedCount(listHostsAll);
		String cap = String.format(getResources()
				.getString(R.string.btn_delete), count);
		barHosts.setButton1Text(cap);
		barHosts.setVisibility(count == 0 ? View.GONE : View.VISIBLE);
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
						loadHosts();
						sbHosts.setText("");
						Toast.makeText(HostMainActivity.this,
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
						loadHosts();
						sbHosts.setText("");
						Toast.makeText(HostMainActivity.this,
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

	// [/region]

	// [region] events
	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnLeft:
			finish();
			break;
		case R.id.btnRight:
			sbHosts.setText("");
			loadHosts();
			break;
		case R.id.btnAdd:
			Intent inHost = new Intent(this, HostAddActivity.class);
			startActivityForResult(inHost, RTConsts.REQCODE_HOST);
			break;
		case R.id.btnCancel:
			sbHosts.setText("");
			break;
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

	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent data) {
		if (resultCode != RESULT_OK) {
			return;
		}
		switch (requestCode) {

		case RTConsts.REQCODE_HOST:
			doMergeHosts(data.getStringArrayExtra("hosts"));
			break;

		}
	}

	@Override
	public Fragment replaceFragment() {
		// TODO Auto-generated method stub
		return null;
	}

	// [/region]

}
