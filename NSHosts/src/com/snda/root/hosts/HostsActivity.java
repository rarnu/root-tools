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
	Button btnHostAdd, btnHostDelete, btnHostAdvance ,btnHostSave;

	int mode = 0;

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

		btnHostAdd.setOnClickListener(this);
		btnHostDelete.setOnClickListener(this);
		btnHostSave.setOnClickListener(this);
		btnHostAdvance.setOnClickListener(this);

		loadSystemHostsT();
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
			// TODO: add host
			break;
		case R.id.btnHostDelete:
			int selCnt = ListViewUtils.getListSelectedCount(lvCurrentHosts);
			if (selCnt == 0) {
				Toast.makeText(this, R.string.c_noselection_del, Toast.LENGTH_LONG).show();
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
		}
	}
	
	private void deleteSelectedItems() {
		List<Map<String, String>> selected = ListViewUtils.getListSelectedItems(lvCurrentHosts);
		if (selected != null) {
			if (selected.size() > 0) {
				for (Map<String, String> obj: selected) {
					if (lstHosts.indexOf(obj) != -1) {
						lstHosts.remove(obj);
					}
				}
				adapter = new SimpleAdapter(HostsActivity.this,
						lstHosts, R.layout.host_item, new String[] {
								"IP", "DOMAIN" }, new int[] {
								R.id.tvItem_IP, R.id.tvItem_Domain });
				lvCurrentHosts.setAdapter(adapter);
			}
		}
		
	}

}
