package com.snda.root.hosts;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import android.app.Activity;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.ListView;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;

import com.snda.root.hosts.adapter.HostItemAdapter;
import com.snda.root.hosts.root.RootUtils;

public class DeprecatedHostsActivity extends Activity implements
		OnClickListener {

	ListView lvDeprecatedHosts;
	ProgressBar pbTestHosts;
	TextView tvTestHosts;
	Button btnDeprecatedClean;

	int mode = 0;

	List<Map<String, String>> lstDeprecated = new ArrayList<Map<String, String>>();
	HostItemAdapter adapter = null;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.deprecated_hosts);

		GlobalInstance.deprecatedHosts = null;
		
		btnDeprecatedClean = (Button) findViewById(R.id.btnDeprecatedClean);
		tvTestHosts = (TextView) findViewById(R.id.tvTestHosts);
		pbTestHosts = (ProgressBar) findViewById(R.id.pbTestHosts);
		lvDeprecatedHosts = (ListView) findViewById(R.id.lvDeprecatedHosts);

		btnDeprecatedClean.setText(R.string.btn_testhosts);
		btnDeprecatedClean.setOnClickListener(this);

		lstDeprecated.clear();
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnDeprecatedClean:
			if (mode == 0) {
				// scan
				scanDeprecatedHostsT();
			} else if (mode == 1) {
				// clean
				GlobalInstance.deprecatedHosts = lstDeprecated;
				setResult(RESULT_OK);
				finish();
			}
			break;
		}
	}

	private void scanDeprecatedHostsT() {
		pbTestHosts.setVisibility(View.VISIBLE);
		btnDeprecatedClean.setEnabled(false);

		final Handler h = new Handler() {
			@SuppressWarnings("unchecked")
			@Override
			public void handleMessage(Message msg) {
				switch (msg.what) {
				case 1:
					pbTestHosts.setVisibility(View.GONE);
					tvTestHosts.setText("");
					btnDeprecatedClean.setEnabled(true);
					btnDeprecatedClean.setText(R.string.btn_cleanhosts);
					mode = 1;
					if (lstDeprecated.size() == 0) {
						Toast
								.makeText(DeprecatedHostsActivity.this,
										R.string.c_nodeprecatedhosts,
										Toast.LENGTH_LONG).show();
						finish();
					}
					break;
				case 2:
					tvTestHosts.setText(String.format(getResources().getString(
							R.string.tv_testhost), msg.arg1,
							GlobalInstance.testHosts.size()));
					Map<String, String> m = (Map<String, String>) msg.obj;
					if (m.get("CHECKED").equals("true")) {
						m.put("CHECKED", "false");
					}
					if (lstDeprecated.indexOf(m) == -1) {
						lstDeprecated.add(m);
						adapter = new HostItemAdapter(
								DeprecatedHostsActivity.this, lstDeprecated,
								false);
						lvDeprecatedHosts.setAdapter(adapter);
					}

					break;
				case 3:
					tvTestHosts.setText(String.format(getResources().getString(
							R.string.tv_testhost), msg.arg1,
							GlobalInstance.testHosts.size()));
					break;
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				String ip = "";
				String ping = "";
				int idx = 0;
				for (Map<String, String> m : GlobalInstance.testHosts) {
					idx++;
					ip = m.get("IP");
					ping = RootUtils.ping(ip);
					if (ping.equals("") || ping.equals("timeout")) {
						// fail
						Message msg = new Message();
						msg.what = 2;
						msg.arg1 = idx;
						msg.obj = m;
						h.sendMessage(msg);
					} else {
						// pass
						Message msg = new Message();
						msg.what = 3;
						msg.arg1 = idx;
						h.sendMessage(msg);
					}
				}
				h.sendEmptyMessage(1);
			}
		}).start();

	}
}
