package com.snda.root.hosts;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.util.DisplayMetrics;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.ListView;
import android.widget.RelativeLayout;
import android.widget.Toast;

import com.snda.root.hosts.adapter.HostItemAdapter;
import com.snda.root.hosts.root.RootUtils;
import com.snda.root.hosts.utils.DIPairUtils;
import com.snda.root.hosts.utils.FileUtils;
import com.snda.root.hosts.utils.ListViewUtils;

public class HostsActivity extends Activity implements OnClickListener {

	List<Map<String, String>> lstHosts = null;
	HostItemAdapter adapter = null;

	ListView lvCurrentHosts;
	RelativeLayout layLoadHosts;
	Button btnHostAdd, btnHostDelete, btnHostSelAll, btnHostSelNone;
	Button btnHostClean, btnHostAdvance, btnHostSave;

	int mode = 0;

	DisplayMetrics dm = new DisplayMetrics();

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.hosts);

		mode = getIntent().getIntExtra("mode", 0);
		lvCurrentHosts = (ListView) findViewById(R.id.lvCurrentHosts);
		layLoadHosts = (RelativeLayout) findViewById(R.id.layLoadHosts);
		btnHostAdd = (Button) findViewById(R.id.btnHostAdd);
		btnHostDelete = (Button) findViewById(R.id.btnHostDelete);
		btnHostSave = (Button) findViewById(R.id.btnHostSave);
		btnHostAdvance = (Button) findViewById(R.id.btnHostAdvance);
		btnHostSelAll = (Button) findViewById(R.id.btnHostSelAll);
		btnHostSelNone = (Button) findViewById(R.id.btnHostSelNone);
		btnHostClean = (Button) findViewById(R.id.btnHostClean);

		btnHostAdd.setOnClickListener(this);
		btnHostDelete.setOnClickListener(this);
		btnHostSave.setOnClickListener(this);
		btnHostAdvance.setOnClickListener(this);
		btnHostSelAll.setOnClickListener(this);
		btnHostSelNone.setOnClickListener(this);
		btnHostClean.setOnClickListener(this);

		resizeCompleteButtons();

		loadSystemHostsT();
		if (mode == 1) {
			setResult(RESULT_OK);
		}
	}

	private void resizeCompleteButtons() {

		getWindowManager().getDefaultDisplay().getMetrics(dm);

		int w = getWindowManager().getDefaultDisplay().getWidth();

		int btnw = (w - dipToPx(8)) / 4;

		RelativeLayout.LayoutParams lp = (RelativeLayout.LayoutParams) btnHostAdd
				.getLayoutParams();
		lp.width = btnw;
		btnHostAdd.setLayoutParams(lp);

		lp = (RelativeLayout.LayoutParams) btnHostDelete.getLayoutParams();
		lp.width = btnw;
		btnHostDelete.setLayoutParams(lp);

		lp = (RelativeLayout.LayoutParams) btnHostSelAll.getLayoutParams();
		lp.width = btnw;
		btnHostSelAll.setLayoutParams(lp);

		lp = (RelativeLayout.LayoutParams) btnHostSelNone.getLayoutParams();
		lp.width = btnw;
		btnHostSelNone.setLayoutParams(lp);

		btnw = (w - dipToPx(8)) / 3;

		lp = (RelativeLayout.LayoutParams) btnHostClean.getLayoutParams();
		lp.width = btnw;
		btnHostClean.setLayoutParams(lp);

		lp = (RelativeLayout.LayoutParams) btnHostAdvance.getLayoutParams();
		lp.width = btnw;
		btnHostAdvance.setLayoutParams(lp);

		lp = (RelativeLayout.LayoutParams) btnHostSave.getLayoutParams();
		lp.width = btnw;
		btnHostSave.setLayoutParams(lp);

	}

	public int dipToPx(int dip) {
		return (int) (dip * dm.density + 0.5f);
	}

	private void loadSystemHostsT() {
		lvCurrentHosts.setVisibility(View.GONE);
		layLoadHosts.setVisibility(View.VISIBLE);

		final Handler h = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					lvCurrentHosts.setAdapter(adapter);
					ListViewUtils.setListSelected(lvCurrentHosts, lstHosts,
							false);
					layLoadHosts.setVisibility(View.GONE);
					lvCurrentHosts.setVisibility(View.VISIBLE);

				}
				super.handleMessage(msg);
			}

		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				lstHosts = DIPairUtils.toPairList("/system/etc/hosts");
				if (lstHosts != null) {
					if (lstHosts.size() > 0) {
						adapter = new HostItemAdapter(HostsActivity.this,
								lstHosts, true);
					}
				} else {
					lstHosts = null;
					adapter = null;
				}

				if (mode == 1) {
					if (lstHosts == null) {
						lstHosts = new ArrayList<Map<String, String>>();
					}
					if (GlobalInstance.passedHosts != null) {
						DIPairUtils.mergePairLists(lstHosts,
								GlobalInstance.passedHosts);
					}
				}

				h.sendEmptyMessage(1);

			}
		}).start();
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnHostAdd:
			// add host
			Intent inAddHost = new Intent(this, AddHostActivity.class);
			startActivityForResult(inAddHost, 0);

			break;
		case R.id.btnHostDelete:
			int selCnt = ListViewUtils.getListSelectedCount(lstHosts);
			if (selCnt == 0) {
				Toast.makeText(this, R.string.c_noselection_del,
						Toast.LENGTH_LONG).show();
			} else {
				deleteSelectedItems();
			}
			break;
		case R.id.btnHostSave:
			// save hosts
			saveHostsT();
			break;
		case R.id.btnHostAdvance:
			// advanced editing
			GlobalInstance.hostsText = buildHostsText();
			Intent inEdit = new Intent(this, EditHostsActivity.class);
			startActivityForResult(inEdit, 1);
			break;
		case R.id.btnHostSelAll:
			ListViewUtils.setListSelected(lvCurrentHosts, lstHosts, true);
			break;
		case R.id.btnHostSelNone:
			ListViewUtils.setListSelected(lvCurrentHosts, lstHosts, false);
			break;
		case R.id.btnHostClean:
			// clean deprecated hosts
			GlobalInstance.testHosts = lstHosts;
			if (GlobalInstance.testHosts == null
					|| GlobalInstance.testHosts.size() == 0) {
				Toast
						.makeText(this, R.string.c_nohostsclean,
								Toast.LENGTH_LONG).show();
				return;
			}
			Intent inTest = new Intent(this, DeprecatedHostsActivity.class);
			startActivityForResult(inTest, 2);
			break;
		}
	}

	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent data) {

		if (resultCode == RESULT_OK) {
			switch (requestCode) {
			case 0:
				Map<String, String> m = new HashMap<String, String>();
				m.put("IP", data.getStringExtra("IP"));
				m.put("DOMAIN", data.getStringExtra("DOMAIN"));
				m.put("CHECKED", "false");
				if (lstHosts.indexOf(m) == -1) {
					lstHosts.add(m);
					adapter = new HostItemAdapter(HostsActivity.this, lstHosts, true);
					lvCurrentHosts.setAdapter(adapter);
				} else {
					Toast.makeText(this, R.string.c_alreadyexists,
							Toast.LENGTH_LONG).show();
				}
				break;
			case 1:
				loadSystemHostsT();
				break;
			case 2:
				// hosts cleaned
				ListViewUtils.setListSelected(lvCurrentHosts, lstHosts, false);
				if (GlobalInstance.deprecatedHosts == null || GlobalInstance.deprecatedHosts.size() == 0) {
					return;
				}
				for (Map<String, String> map: GlobalInstance.deprecatedHosts) {
					if (lstHosts.indexOf(map) != -1) {
						lstHosts.remove(map);
					}
				}
				adapter = new HostItemAdapter(HostsActivity.this, lstHosts, true);
				lvCurrentHosts.setAdapter(adapter);
				break;
			}
		}

	}

	private void deleteSelectedItems() {
		List<Map<String, String>> selected = ListViewUtils
				.getListSelectedItems(lstHosts);
		if (selected != null) {
			if (selected.size() > 0) {
				for (Map<String, String> obj : selected) {
					if (lstHosts.indexOf(obj) != -1) {
						lstHosts.remove(obj);
					}
				}
				adapter = new HostItemAdapter(HostsActivity.this, lstHosts,
						true);
				lvCurrentHosts.setAdapter(adapter);
			}
		}

	}

	private void saveHostsT() {
		setButtonsEnabled(false);
		layLoadHosts.setVisibility(View.VISIBLE);
		layLoadHosts.bringToFront();

		final Handler h = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {

					// push to system
					boolean pushed = RootUtils.pushFileToSystem(
							GlobalInstance.app_dir
									+ GlobalInstance.host_filename,
							GlobalInstance.sys_dir,
							GlobalInstance.host_filename);
					mode = 0;
					loadSystemHostsT();
					setButtonsEnabled(true);

					Toast.makeText(
							HostsActivity.this,
							pushed ? R.string.c_savehostsok
									: R.string.c_savehostsfail,
							Toast.LENGTH_LONG).show();

				} else if (msg.what == 2) {
					setButtonsEnabled(true);
					Toast.makeText(HostsActivity.this,
							R.string.c_savehostsfail, Toast.LENGTH_LONG).show();
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {

				String hostsText = buildHostsText();
				int what = 1;
				try {
					FileUtils.rewriteFile(GlobalInstance.app_dir
							+ GlobalInstance.host_filename, hostsText);
				} catch (IOException e) {
					what = 2;
				}

				h.sendEmptyMessage(what);

			}
		}).start();
	}

	private void setButtonsEnabled(boolean enable) {
		btnHostAdd.setEnabled(enable);
		btnHostDelete.setEnabled(enable);
		btnHostAdvance.setEnabled(enable);
		btnHostClean.setEnabled(enable);
		btnHostSave.setEnabled(enable);
		btnHostSelAll.setEnabled(enable);
		btnHostSelNone.setEnabled(enable);
	}

	private String buildHostsText() {
		String result = "";
		if (lstHosts == null || lstHosts.size() == 0) {
			return "127.0.0.1 localhost";
		}
		for (Map<String, String> m : lstHosts) {
			result = result
					+ String.format("%s %s\n", m.get("IP"), m.get("DOMAIN"));
		}
		return result;
	}

}
