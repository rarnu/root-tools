package com.rarnu.tools.root;

import java.util.ArrayList;
import java.util.List;

import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ListView;
import android.widget.Toast;

import com.rarnu.tools.root.adapter.HostsAdapter;
import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.base.BaseActivity;
import com.rarnu.tools.root.common.HostRecordInfo;
import com.rarnu.tools.root.comp.DataProgressBar;
import com.rarnu.tools.root.utils.DIPairUtils;
import com.rarnu.tools.root.utils.PingUtils;

public class HostDeprecatedActivity extends BaseActivity implements
		OnClickListener {

	// [region] field define

	ListView lvDeprecatedHosts;
	DataProgressBar progressDeprecated;
	// [/region]

	// [region] variable define
	int mode = 0;
	List<HostRecordInfo> lstDeprecated = new ArrayList<HostRecordInfo>();
	HostsAdapter adapter = null;
	// [/region]

	// [region] handler define
	private Handler hSelectHost = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			super.handleMessage(msg);
		};

	};

	// [/region]

	// [region] life circle
	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.layout_host_deprecated);
		init();
		loadHosts();
		LogApi.logEnterDeprecatedHosts();
	}

	// [/region]

	// [region] business logic
	private void loadHosts() {
		progressDeprecated.setAppName(getString(R.string.loading));
		progressDeprecated.setVisibility(View.VISIBLE);

		final Handler h = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					if (lstDeprecated != null) {
						adapter = new HostsAdapter(getLayoutInflater(),
								lstDeprecated, hSelectHost, false, false);
					} else {
						adapter = null;
					}
					lvDeprecatedHosts.setAdapter(adapter);
					progressDeprecated.setVisibility(View.GONE);
				}
				super.handleMessage(msg);
			}

		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				lstDeprecated = DIPairUtils.getHostList();
				h.sendEmptyMessage(1);

			}
		}).start();
	}

	private void scanDeprecatedHostsT() {
		LogApi.logCleanDeprecatedHosts();
		progressDeprecated.setAppName(getString(R.string.testing));
		progressDeprecated.setVisibility(View.VISIBLE);
		btnRight.setEnabled(false);

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {

				if (msg.what == 1) {
					progressDeprecated.setVisibility(View.GONE);
					adapter.notifyDataSetChanged();
					btnRight.setEnabled(true);
					boolean ret = DIPairUtils.saveHosts(lstDeprecated);
					if (ret) {
						Toast.makeText(HostDeprecatedActivity.this,
								R.string.save_hosts_succ, Toast.LENGTH_LONG)
								.show();
						finish();
					} else {
						Toast.makeText(HostDeprecatedActivity.this,
								R.string.save_hosts_error, Toast.LENGTH_LONG)
								.show();
					}
				} else if (msg.what == 2) {
					progressDeprecated.setProgress((String) msg.obj);
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				String ping = "";
				int count = lstDeprecated.size();
				for (int i = count - 1; i >= 0; i--) {
					Message msg = new Message();
					msg.what = 2;
					msg.obj = lstDeprecated.get(i).ip;
					h.sendMessage(msg);

					ping = PingUtils.ping(lstDeprecated.get(i).ip);
					if (ping.equals("") || ping.equals("timeout")) {
						lstDeprecated.remove(i);
					}
				}
				h.sendEmptyMessage(1);
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
			if (mode == 0) {
				// scan
				scanDeprecatedHostsT();
			} else if (mode == 1) {
				// clean
				setResult(RESULT_OK);
				finish();
			}
			break;
		}
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

		lvDeprecatedHosts = (ListView) findViewById(R.id.lvDeprecatedHosts);
		progressDeprecated = (DataProgressBar) findViewById(R.id.progressDeprecated);

	}

	@Override
	public void initTitle() {

		tvName.setText(R.string.clean_deprecated_hosts);
		btnLeft.setText(R.string.back);
		btnLeft.setVisibility(View.VISIBLE);
		btnRight.setText(R.string.clean);
		btnRight.setVisibility(View.VISIBLE);

		// tbTitle.setText(getString(R.string.clean_deprecated_hosts));
		// tbTitle.setLeftButtonText(getString(R.string.back));
		// tbTitle.setRightButtonText(getString(R.string.clean));
		// tbTitle.getLeftButton().setVisibility(View.VISIBLE);
		// tbTitle.getRightButton().setVisibility(View.VISIBLE);

	}

	@Override
	public void initSearchBar() {

	}

	@Override
	public void initEvents() {
		btnRight.setOnClickListener(this);
		btnLeft.setOnClickListener(this);

	}

	// [/region]
}
