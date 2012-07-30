package com.rarnu.tools.root;

import java.util.ArrayList;
import java.util.List;

import android.content.Intent;
import android.content.pm.PackageInfo;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ListView;

import com.rarnu.tools.root.adapter.CompPackageAdapter;
import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.base.BaseActivity;
import com.rarnu.tools.root.comp.DataProgressBar;
import com.rarnu.tools.root.comp.SearchBar;

public class CompMainActivity extends BaseActivity implements OnClickListener,
		OnItemClickListener {

	// [region] field define

	ListView lvComp;
	SearchBar sbComp;
	DataProgressBar progressComp;
	// [/region]

	// [region] variable define
	List<PackageInfo> listCompAll = new ArrayList<PackageInfo>();
	CompPackageAdapter compAdapter = null;

	// [/region]

	// [region] life circle
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.layout_comp);
		init();
		loadCompAppList();
		LogApi.logEnterComponent();
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

		progressComp = (DataProgressBar) findViewById(R.id.progressComp);
		sbComp = (SearchBar) findViewById(R.id.sbComp);
		lvComp = (ListView) findViewById(R.id.lvComp);

	}

	@Override
	public void initTitle() {
		tvName.setText(R.string.func3_title);
		btnLeft.setText(R.string.back);
		btnLeft.setVisibility(View.VISIBLE);
		btnRight.setText(R.string.refresh);
		btnRight.setVisibility(View.VISIBLE);

		// tbTitle.setLeftButtonText(getString(R.string.back));
		// tbTitle.setRightButtonText(getString(R.string.refresh));
		// tbTitle.setText(getString(R.string.func3_title));
		// tbTitle.getLeftButton().setVisibility(View.VISIBLE);
		// tbTitle.getRightButton().setVisibility(View.VISIBLE);
	}

	@Override
	public void initSearchBar() {
		sbComp.setAddButtonVisible(false);

	}

	@Override
	public void initEvents() {
		btnLeft.setOnClickListener(this);
		btnRight.setOnClickListener(this);
		sbComp.getCancelButton().setOnClickListener(this);
		sbComp.getEditText().addTextChangedListener(new TextWatcher() {

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
				if (s == null || compAdapter == null) {
					return;
				}
				compAdapter.getFilter().filter(sbComp.getText().toString());
			}
		});
		lvComp.setOnItemClickListener(this);
	}

	// [/region]

	// [region] business logic
	private void loadCompAppList() {
		progressComp.setAppName(getString(R.string.loading));
		progressComp.setVisibility(View.VISIBLE);
		btnRight.setEnabled(false);

		final Handler h = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					if (listCompAll != null) {
						compAdapter = new CompPackageAdapter(
								getLayoutInflater(), listCompAll);
					} else {
						compAdapter = null;
					}
					lvComp.setAdapter(compAdapter);
					progressComp.setVisibility(View.GONE);
					btnRight.setEnabled(true);

				} else if (msg.what == 2) {
					progressComp.setProgress(String.format("%d/%d", msg.arg1,
							msg.arg2));
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			public void run() {
				listCompAll = GlobalInstance.pm.getInstalledPackages(0);
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
			sbComp.setText("");
			loadCompAppList();
			break;
		case R.id.btnCancel:
			sbComp.setText("");
			break;
		}
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		GlobalInstance.currentComp = (PackageInfo) lvComp
				.getItemAtPosition(position);

		Intent inPackage = new Intent(this, CompPackageInfoActivity.class);
		startActivity(inPackage);
	}

	// [/region]
}
