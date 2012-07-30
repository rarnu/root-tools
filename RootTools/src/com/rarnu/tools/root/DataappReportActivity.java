package com.rarnu.tools.root;

import java.util.List;

import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ListView;

import com.rarnu.tools.root.adapter.DataappReportAdapter;
import com.rarnu.tools.root.base.BaseActivity;
import com.rarnu.tools.root.common.DataappInfo;
import com.rarnu.tools.root.utils.ApkUtils;

public class DataappReportActivity extends BaseActivity implements
		OnClickListener {

	// [region] field define

	ListView lvReport;

	// [/region]

	// [region] life circle
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.layout_dataapp_report);

		init();

		showReport();
	}

	// [/region]

	// [region] business logic
	private void showReport() {
		List<DataappInfo> report = ApkUtils.getOperationLog();
		DataappReportAdapter adapter = new DataappReportAdapter(
				getLayoutInflater(), report);
		lvReport.setAdapter(adapter);
	}

	// [/region]

	// [region] events
	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnLeft:
			finish();
			break;
		}

	}

	// [/region]

	// [region] init

	@Override
	public void init() {
		mappingTitle();
		mappingComp();
		initSearchBar();
		initTitle();
		initEvents();

	}

	@Override
	public void mappingComp() {

		lvReport = (ListView) findViewById(R.id.lvReport);

	}

	@Override
	public void initTitle() {
		tvName.setText(R.string.operation_result);
		btnLeft.setText(R.string.back);
		btnLeft.setVisibility(View.VISIBLE);

		// tbTitle.setText(getString(R.string.operation_result));
		// tbTitle.setLeftButtonText(getString(R.string.back));
		// tbTitle.getLeftButton().setVisibility(View.VISIBLE);

	}

	@Override
	public void initSearchBar() {

	}

	@Override
	public void initEvents() {
		btnLeft.setOnClickListener(this);

	}
	// [/region]
}
