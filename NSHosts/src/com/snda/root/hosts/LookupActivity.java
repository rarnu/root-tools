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
import android.widget.SimpleAdapter;
import android.widget.Toast;

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

//	DisplayMetrics dm = new DisplayMetrics();

	List<Map<String, String>> listDI = null;
	SimpleAdapter adapter = null;

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

//		resizeCompleteButtons();

		btnLookup.setOnClickListener(this);
		btnLookupSelAll.setOnClickListener(this);
		btnLookupSelNone.setOnClickListener(this);
		btnLookupAddHosts.setOnClickListener(this);
		btnLookupCommon.setOnClickListener(this);
	}

//	private void resizeCompleteButtons() {
//
//		getWindowManager().getDefaultDisplay().getMetrics(dm);
//
//		int w = getWindowManager().getDefaultDisplay().getWidth();
//
//		RelativeLayout.LayoutParams lp = (RelativeLayout.LayoutParams) btnLookup
//				.getLayoutParams();
//		int btnw = (w - lp.width - dipToPx(8)) / 3;
//
//		lp = (RelativeLayout.LayoutParams) btnCom.getLayoutParams();
//		lp.width = btnw;
//		btnCom.setLayoutParams(lp);
//
//		lp = (RelativeLayout.LayoutParams) btnOrg.getLayoutParams();
//		lp.width = btnw;
//		btnOrg.setLayoutParams(lp);
//
//		lp = (RelativeLayout.LayoutParams) btnCn.getLayoutParams();
//		lp.width = btnw;
//		btnCn.setLayoutParams(lp);
//
//	}

//	public int dipToPx(int dip) {
//		return (int) (dip * dm.density + 0.5f);
//	}

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
//		case R.id.btnCom:
//			if (!etSiteName.getText().toString().endsWith(".com")) {
//				etSiteName.setText(etSiteName.getText().toString() + ".com");
//			}
//			break;
//		case R.id.btnOrg:
//			if (!etSiteName.getText().toString().endsWith(".org")) {
//				etSiteName.setText(etSiteName.getText().toString() + ".org");
//			}
//			break;
//		case R.id.btnCn:
//			if (!etSiteName.getText().toString().endsWith(".cn")) {
//				etSiteName.setText(etSiteName.getText().toString() + ".cn");
//			}
//			break;
		case R.id.btnLookupSelAll:
			ListViewUtils.setListSelected(lvLookupResult, true);
			break;
		case R.id.btnLookupSelNone:
			ListViewUtils.setListSelected(lvLookupResult, false);
			break;
		case R.id.btnLookupAddHosts:
			List<Map<String, String>> lst = ListViewUtils.getListSelectedItems(lvLookupResult);
			if (lst.size() > 0) {
				GlobalInstance.passedHosts = lst;
				Intent inHosts = new Intent(this, HostsActivity.class);
				inHosts.putExtra("mode", 1);
				startActivity(inHosts);
			} else {
				GlobalInstance.passedHosts = null;
				Toast.makeText(this, R.string.c_noselection_add, Toast.LENGTH_LONG).show();
			}
			break;
		case R.id.btnLookupCommon:
			// TODO: common google sites
			break;
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

					layLookupProcess.setVisibility(View.GONE);
					lvLookupResult.setVisibility(View.VISIBLE);

				}
				super.handleMessage(msg);
			}

		};

		new Thread(new Runnable() {

			@Override
			public void run() {

				List<Address> list = NSLookup.nslookup(etSiteName.getText()
						.toString(), "8.8.8.8");
				if (list != null) {
					listDI = DIPairUtils.toPairList(etSiteName.getText().toString(),list);
					if (listDI != null) {
						adapter = new SimpleAdapter(LookupActivity.this,
								listDI, R.layout.host_item, new String[] {
										"IP", "DOMAIN" }, new int[] {
										R.id.tvItem_IP, R.id.tvItem_Domain });
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
