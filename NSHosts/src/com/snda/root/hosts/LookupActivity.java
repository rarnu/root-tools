package com.snda.root.hosts;

import java.util.List;
import java.util.Map;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.RelativeLayout;
import android.widget.Toast;

import com.snda.root.hosts.adapter.HostItemAdapter;
import com.snda.root.hosts.dns.NSLookup;
import com.snda.root.hosts.dns.record.Address;
import com.snda.root.hosts.utils.DIPairUtils;
import com.snda.root.hosts.utils.ListViewUtils;

public class LookupActivity extends Activity implements OnClickListener {

	Button btnLookup;
	Button btnLookupSelAll, btnLookupSelNone;
	Button btnLookupAddHosts;
	Button btnLookupCommon;
	EditText etSiteName;

	ListView lvLookupResult;
	RelativeLayout layLookupProcess;

	// DisplayMetrics dm = new DisplayMetrics();

	List<Map<String, String>> listDI = null;
	HostItemAdapter adapter = null;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.nslookup);

		btnLookup = (Button) findViewById(R.id.btnLookup);
		etSiteName = (EditText) findViewById(R.id.etSiteName);
		lvLookupResult = (ListView) findViewById(R.id.lvLookupResult);
		layLookupProcess = (RelativeLayout) findViewById(R.id.layLookupProcess);
		btnLookupSelAll = (Button) findViewById(R.id.btnLookupSelAll);
		btnLookupSelNone = (Button) findViewById(R.id.btnLookupSelNone);
		btnLookupAddHosts = (Button) findViewById(R.id.btnLookupAddHosts);
		btnLookupCommon = (Button) findViewById(R.id.btnLookupCommon);

		btnLookup.setOnClickListener(this);
		btnLookupSelAll.setOnClickListener(this);
		btnLookupSelNone.setOnClickListener(this);
		btnLookupAddHosts.setOnClickListener(this);
		btnLookupCommon.setOnClickListener(this);
		setButtonsEnabled(false);

	}

	private void setButtonsEnabled(boolean enable) {
		btnLookupSelAll.setEnabled(enable);
		btnLookupSelNone.setEnabled(enable);
		btnLookupAddHosts.setEnabled(enable);
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnLookup:
			if (etSiteName.getText().toString().equals("")) {
				Toast
						.makeText(this, R.string.c_input_domain,
								Toast.LENGTH_LONG).show();
				return;
			}

			getHostsT();
			break;

		case R.id.btnLookupSelAll:
			ListViewUtils.setListSelected(lvLookupResult, listDI, true);
			break;
		case R.id.btnLookupSelNone:
			ListViewUtils.setListSelected(lvLookupResult, listDI, false);
			break;
		case R.id.btnLookupAddHosts:
			List<Map<String, String>> lst = ListViewUtils
					.getListSelectedItems(listDI);
			if (lst.size() > 0) {
				GlobalInstance.passedHosts = lst;
				Intent inHosts = new Intent(this, HostsActivity.class);
				inHosts.putExtra("mode", 1);
				startActivityForResult(inHosts, 1);
			} else {
				GlobalInstance.passedHosts = null;
				Toast.makeText(this, R.string.c_noselection_add,
						Toast.LENGTH_LONG).show();
			}
			break;
		case R.id.btnLookupCommon:
			// common google sites
			Intent inSites = new Intent(this, CommonSiteActivity.class);
			startActivityForResult(inSites, 0);
			break;
		}
	}

	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent data) {
		if (resultCode == RESULT_OK) {
			switch (requestCode) {
			case 0:
				etSiteName.setText(data.getStringExtra("DOMAIN"));
				break;
			case 1:
				ListViewUtils.setListSelected(lvLookupResult, listDI,
						GlobalInstance.autoSelect);
				break;
			}
		}
	}

	private void getHostsT() {
		lvLookupResult.setVisibility(View.GONE);
		layLookupProcess.setVisibility(View.VISIBLE);

		final Handler h = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					lvLookupResult.setAdapter(adapter);
					setButtonsEnabled(adapter != null);
					layLookupProcess.setVisibility(View.GONE);
					lvLookupResult.setVisibility(View.VISIBLE);
					etSiteName.setText("");

				}
				super.handleMessage(msg);
			}

		};

		new Thread(new Runnable() {

			@Override
			public void run() {

				List<Address> list = NSLookup.nslookup(etSiteName.getText()
						.toString(), GlobalInstance.nameServer);
				if (list != null) {
					listDI = DIPairUtils.toPairList(etSiteName.getText()
							.toString(), list);
					if (listDI != null) {
						adapter = new HostItemAdapter(LookupActivity.this,
								listDI, true);
					} else {
						adapter = null;
					}

				} else {
					listDI = null;
					adapter = null;
				}
				h.sendEmptyMessage(1);
			}
		}).start();
	}
}
