package com.snda.root.hosts;

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
import android.widget.SimpleAdapter;
import android.widget.Toast;

import com.snda.root.hosts.utils.DIPairUtils;
import com.snda.root.hosts.utils.ListViewUtils;

public class HostsActivity extends Activity implements OnClickListener {

	List<Map<String, String>> lstHosts = null;
	SimpleAdapter adapter = null;

	ListView lvCurrentHosts;
	RelativeLayout layLoadHosts;
	Button btnHostAdd, btnHostDelete, btnHostSelAll, btnHostSelNone;
	Button btnHostClean,btnHostAdvance, btnHostSave;

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
	}

	private void resizeCompleteButtons() {

		getWindowManager().getDefaultDisplay().getMetrics(dm);

		int w = getWindowManager().getDefaultDisplay().getWidth();

		int btnw = (w - dipToPx(8)) / 4;

		RelativeLayout.LayoutParams lp = (RelativeLayout.LayoutParams) btnHostAdd.getLayoutParams();
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
					layLoadHosts.setVisibility(View.GONE);
					lvCurrentHosts.setVisibility(View.VISIBLE);

				}
				super.handleMessage(msg);
			}

		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				lstHosts = DIPairUtils.toPairList("/etc/hosts");
				if (lstHosts != null) {
					if (lstHosts.size() > 0) {
						adapter = new SimpleAdapter(HostsActivity.this,
								lstHosts, R.layout.host_item, new String[] {
										"IP", "DOMAIN" }, new int[] {
										R.id.tvItem_IP, R.id.tvItem_Domain });
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
			int selCnt = ListViewUtils.getListSelectedCount(lvCurrentHosts);
			if (selCnt == 0) {
				Toast.makeText(this, R.string.c_noselection_del,
						Toast.LENGTH_LONG).show();
			} else {
				deleteSelectedItems();
			}
			break;
		case R.id.btnHostSave:
			// TODO: save hosts
			break;
		case R.id.btnHostAdvance:
			// TODO: advanced editing
			break;
		case R.id.btnHostSelAll:
			ListViewUtils.setListSelected(lvCurrentHosts, true);
			break;
		case R.id.btnHostSelNone:
			ListViewUtils.setListSelected(lvCurrentHosts, false);
			break;
		case R.id.btnHostClean:
			// TODO: clean deprecated hosts
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
				if (lstHosts.indexOf(m) == -1) {
					lstHosts.add(m);
					adapter = new SimpleAdapter(HostsActivity.this, lstHosts,
							R.layout.host_item,
							new String[] { "IP", "DOMAIN" }, new int[] {
									R.id.tvItem_IP, R.id.tvItem_Domain });
					lvCurrentHosts.setAdapter(adapter);
				} else {
					Toast.makeText(this, R.string.c_alreadyexists,
							Toast.LENGTH_LONG).show();
				}
				break;
			}
		}

	}

	private void deleteSelectedItems() {
		List<Map<String, String>> selected = ListViewUtils
				.getListSelectedItems(lvCurrentHosts);
		if (selected != null) {
			if (selected.size() > 0) {
				for (Map<String, String> obj : selected) {
					if (lstHosts.indexOf(obj) != -1) {
						lstHosts.remove(obj);
					}
				}
				adapter = new SimpleAdapter(HostsActivity.this, lstHosts,
						R.layout.host_item, new String[] { "IP", "DOMAIN" },
						new int[] { R.id.tvItem_IP, R.id.tvItem_Domain });
				lvCurrentHosts.setAdapter(adapter);
			}
		}

	}

}
