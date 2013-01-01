package com.rarnu.tools.root;

import java.util.List;

import android.app.Fragment;
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

	

	
	public void mappingComp() {

		lvReport = (ListView) findViewById(R.id.lvReport);

	}


	// [/region]

	@Override
	public Fragment replaceFragment() {
		// TODO Auto-generated method stub
		return null;
	}
}
