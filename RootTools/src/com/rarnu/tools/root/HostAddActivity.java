package com.rarnu.tools.root;

import java.util.List;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.text.InputFilter;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.BaseAdapter;
import android.widget.Button;
import android.widget.ListView;
import android.widget.Toast;

import com.rarnu.tools.root.adapter.HostsAdapter;
import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.base.BaseActivity;
import com.rarnu.tools.root.common.HostRecordInfo;
import com.rarnu.tools.root.comp.DataBar;
import com.rarnu.tools.root.comp.DataProgressBar;
import com.rarnu.tools.root.comp.SearchBar;
import com.rarnu.tools.root.dns.NSLookup;
import com.rarnu.tools.root.dns.record.Address;
import com.rarnu.tools.root.utils.DIPairUtils;

@SuppressLint("HandlerLeak")
public class HostAddActivity extends BaseActivity implements OnClickListener {

	// [region] field define

	SearchBar sbSearchHosts;
	ListView lvAddHosts;
	DataBar barAddHosts;
	DataProgressBar progressSearchHosts;
	Button btnCom, btnOrg, btnNet, btnEdu, btnInfo, btnBiz, btnCn, btnUs,
			btnJp, btnHk, btnTw;
	// [/region]

	// [region] variable define
	HostsAdapter adapter = null;
	List<HostRecordInfo> list = null;
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
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.layout_host_add);

		init();
	}

	// [/region]

	// [region] events
	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnLeft:
			finish();
			return;
		case R.id.btnRight:
			returnAddHosts();
			return;
		case R.id.btnCancel:
			// search
			String domain = sbSearchHosts.getText().toString();
			if (domain == null || domain.equals("")) {
				Toast.makeText(this, R.string.domain_name_empty,
						Toast.LENGTH_LONG).show();
				return;
			}
			searchHosts(domain);
			return;
		case R.id.barButton1:
			deleteSelectedItemsFromList();
			return;
		case R.id.barButton2:
			setHostItemSelectedStatus(list, adapter, hSelectHost, false);
			return;

		case R.id.btnCom:
			sbSearchHosts.setText(sbSearchHosts.getText() + ".com");

			break;
		case R.id.btnOrg:
			sbSearchHosts.setText(sbSearchHosts.getText() + ".org");
			break;
		case R.id.btnNet:
			sbSearchHosts.setText(sbSearchHosts.getText() + ".net");
			break;
		case R.id.btnEdu:
			sbSearchHosts.setText(sbSearchHosts.getText() + ".edu");
			break;
		case R.id.btnInfo:
			sbSearchHosts.setText(sbSearchHosts.getText() + ".info");
			break;
		case R.id.btnBiz:
			sbSearchHosts.setText(sbSearchHosts.getText() + ".biz");
			break;
		case R.id.btnCn:
			sbSearchHosts.setText(sbSearchHosts.getText() + ".cn");
			break;
		case R.id.btnUs:
			sbSearchHosts.setText(sbSearchHosts.getText() + ".us");
			break;
		case R.id.btnJp:
			sbSearchHosts.setText(sbSearchHosts.getText() + ".jp");
			break;
		case R.id.btnHk:
			sbSearchHosts.setText(sbSearchHosts.getText() + ".hk");
			break;
		case R.id.btnTw:
			sbSearchHosts.setText(sbSearchHosts.getText() + ".tw");
			break;
		case R.id.chkSelAll:
			boolean selected = barAddHosts.getCheckBox().isChecked();
			setHostItemSelectedStatus(list, adapter, hSelectHost, selected);
			break;
		}

		sbSearchHosts.getEditText().setSelection(
				sbSearchHosts.getText().toString().length());
		sbSearchHosts.getEditText().requestFocus();
	}

	// [/region]

	// [region] business logic

	private void searchHosts(final String domain) {

		LogApi.logSearchHosts(domain);
		barAddHosts.setVisibility(View.GONE);
		progressSearchHosts.setAppName(getString(R.string.searching));
		progressSearchHosts.setVisibility(View.VISIBLE);

		final Handler h = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					if (list == null) {
						adapter = null;
					} else {
						adapter = new HostsAdapter(getLayoutInflater(), list,
								hSelectHost, false, true);
					}

					lvAddHosts.setAdapter(adapter);
					sbSearchHosts.setText("");
					progressSearchHosts.setVisibility(View.GONE);
					showHostSelectedCount();
				}
				super.handleMessage(msg);
			}

		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				List<Address> listAddress = NSLookup.nslookup(domain,
						GlobalInstance.nameServer);
				list = DIPairUtils.toPairList(domain, listAddress);
				h.sendEmptyMessage(1);
			}
		}).start();
	}

	private void showHostSelectedCount() {
		int count = getHostSelectedCount(list);
		String cap = String.format(getResources()
				.getString(R.string.btn_delete), count);
		barAddHosts.setButton1Text(cap);
		barAddHosts.setVisibility(count == 0 ? View.GONE : View.VISIBLE);
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

	private void deleteSelectedItemsFromList() {
		int count = list.size();
		for (int i = count - 1; i >= 0; i--) {
			if (list.get(i).checked) {
				list.remove(i);
			}
		}
		adapter.notifyDataSetChanged();
		showHostSelectedCount();
	}

	private void returnAddHosts() {
		if (list == null || list.size() == 0) {
			Toast.makeText(this, R.string.no_host_for_add, Toast.LENGTH_LONG)
					.show();
			return;
		}
		String host = "";
		String[] strHosts = new String[list.size()];
		int cnt = 0;
		for (HostRecordInfo info : list) {
			host = String.format("%s\t%s", info.ip, info.domain);
			strHosts[cnt] = new String(host);
			cnt++;
		}

		Intent inReturn = new Intent();
		inReturn.putExtra("hosts", strHosts);
		setResult(RESULT_OK, inReturn);
		finish();
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

		sbSearchHosts = (SearchBar) findViewById(R.id.sbSearchHosts);
		lvAddHosts = (ListView) findViewById(R.id.lvAddHosts);
		barAddHosts = (DataBar) findViewById(R.id.barAddHosts);
		progressSearchHosts = (DataProgressBar) findViewById(R.id.progressSearchHosts);

		btnCom = (Button) findViewById(R.id.btnCom);
		btnOrg = (Button) findViewById(R.id.btnOrg);
		btnNet = (Button) findViewById(R.id.btnNet);
		btnEdu = (Button) findViewById(R.id.btnEdu);
		btnInfo = (Button) findViewById(R.id.btnInfo);
		btnBiz = (Button) findViewById(R.id.btnBiz);
		btnCn = (Button) findViewById(R.id.btnCn);
		btnUs = (Button) findViewById(R.id.btnUs);
		btnJp = (Button) findViewById(R.id.btnJp);
		btnHk = (Button) findViewById(R.id.btnHk);
		btnTw = (Button) findViewById(R.id.btnTw);

	}

	@Override
	public void initTitle() {
		tvName.setText(R.string.host_add);
		btnLeft.setText(R.string.back);
		btnLeft.setVisibility(View.VISIBLE);
		btnRight.setText(R.string.add);
		btnRight.setVisibility(View.VISIBLE);

		// tbTitle.setText(getString(R.string.host_add));
		// tbTitle.setLeftButtonText(getString(R.string.back));
		// tbTitle.setRightButtonText(getString(R.string.add));
		// tbTitle.getLeftButton().setVisibility(View.VISIBLE);
		// tbTitle.getRightButton().setVisibility(View.VISIBLE);

	}

	@Override
	public void initSearchBar() {
		sbSearchHosts.getEditText().setHint(R.string.tv_sitename);
		sbSearchHosts.getEditText().setFilters(
				new InputFilter[] { new InputFilter.LengthFilter(32) });
		sbSearchHosts.setAddButtonVisible(false);
		sbSearchHosts.getCancelButton()
				.setBackgroundResource(R.drawable.search);
		barAddHosts.setCheckBoxVisible(true);
	}

	@Override
	public void initEvents() {
		sbSearchHosts.getCancelButton().setOnClickListener(this);
		barAddHosts.getButton1().setOnClickListener(this);
		barAddHosts.getButton2().setOnClickListener(this);
		btnCom.setOnClickListener(this);
		btnOrg.setOnClickListener(this);
		btnNet.setOnClickListener(this);
		btnEdu.setOnClickListener(this);
		btnInfo.setOnClickListener(this);
		btnBiz.setOnClickListener(this);
		btnCn.setOnClickListener(this);
		btnUs.setOnClickListener(this);
		btnJp.setOnClickListener(this);
		btnHk.setOnClickListener(this);
		btnTw.setOnClickListener(this);
		btnLeft.setOnClickListener(this);
		btnRight.setOnClickListener(this);
		barAddHosts.getCheckBox().setOnClickListener(this);
	}

	// [/region]

}
