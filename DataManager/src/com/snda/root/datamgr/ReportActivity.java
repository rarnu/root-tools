package com.snda.root.datamgr;

import java.util.List;

import android.app.Activity;
import android.os.Bundle;
import android.widget.ListView;

import com.snda.root.datamgr.adapter.ReportAdapter;
import com.snda.root.datamgr.utils.ApkUtils;
import com.snda.root.datamgr.utils.AppInfo;

public class ReportActivity extends Activity {

	ListView lvReport;
	
	
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.report);
		
		lvReport = (ListView) findViewById(R.id.lvReport);
		showReport();
	}
	
	
	private void showReport() {
		List<AppInfo> report = ApkUtils.getOperationLog();
		ReportAdapter adapter = new ReportAdapter(getLayoutInflater(), report);
		lvReport.setAdapter(adapter);
	}
}
