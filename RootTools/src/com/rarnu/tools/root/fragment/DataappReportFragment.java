package com.rarnu.tools.root.fragment;

import java.util.List;

import android.os.Bundle;
import android.view.Menu;
import android.widget.GridView;
import android.widget.ListView;

import com.rarnu.devlib.base.BasePopupFragment;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.DataappReportAdapter;
import com.rarnu.tools.root.common.DataappInfo;
import com.rarnu.tools.root.utils.ApkUtils;

public class DataappReportFragment extends BasePopupFragment {

	ListView lvReport;
	GridView gvReport;

	@Override
	protected int getBarTitle() {
		return R.string.operation_result;
	}

	@Override
	protected int getBarTitleWithPath() {
		return R.string.operation_result;
	}

	@Override
	protected void initComponents() {
		lvReport = (ListView) innerView.findViewById(R.id.lvReport);
		gvReport = (GridView) innerView.findViewById(R.id.gvReport);
	}

	@Override
	protected void initLogic() {
		showReport();

	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.layout_dataapp_report;
	}

	@Override
	protected void initMenu(Menu menu) {

	}

	private void showReport() {
		List<DataappInfo> report = ApkUtils.getOperationLog();
		DataappReportAdapter adapter = new DataappReportAdapter(getActivity(),
				report);
		if (lvReport != null) {
			lvReport.setAdapter(adapter);
		}
		if (gvReport != null) {
			gvReport.setAdapter(adapter);
		}
	}

	@Override
	protected void initEvents() {
		
	}

	@Override
	protected String getMainActivityName() {
		return MainActivity.class.getName();
	}

	@Override
	protected void onGetNewArguments(Bundle bn) {
		
	}

}
