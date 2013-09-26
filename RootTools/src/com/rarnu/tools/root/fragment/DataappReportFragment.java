package com.rarnu.tools.root.fragment;

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

import java.util.List;

public class DataappReportFragment extends BasePopupFragment {

    ListView lvReport;
    GridView gvReport;

    @Override
    public int getBarTitle() {
        return R.string.operation_result;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.operation_result;
    }

    @Override
    public void initComponents() {
        lvReport = (ListView) innerView.findViewById(R.id.lvReport);
        gvReport = (GridView) innerView.findViewById(R.id.gvReport);
    }

    @Override
    public void initLogic() {
        showReport();

    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_dataapp_report;
    }

    @Override
    public void initMenu(Menu menu) {

    }

    private void showReport() {
        List<DataappInfo> report = ApkUtils.getOperationLog();
        DataappReportAdapter adapter = new DataappReportAdapter(getActivity(), report);
        if (lvReport != null) {
            lvReport.setAdapter(adapter);
        }
        if (gvReport != null) {
            gvReport.setAdapter(adapter);
        }
    }

    @Override
    public void initEvents() {

    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
    }

    @Override
    public void onGetNewArguments(Bundle bn) {

    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

}
