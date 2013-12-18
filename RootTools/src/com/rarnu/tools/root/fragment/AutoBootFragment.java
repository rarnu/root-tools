package com.rarnu.tools.root.fragment;

import android.os.Bundle;
import android.view.Menu;
import android.widget.ListView;
import android.widget.SearchView;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.DataProgressBar;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.utils.ResourceUtils;

public class AutoBootFragment extends BaseFragment {

    ListView lvComp;
    DataProgressBar progressComp;
    SearchView sv;

    public AutoBootFragment() {
        super();
        tabTitle = ResourceUtils.getString(R.string.func_auto_boot);
    }

    @Override
    public int getBarTitle() {
        return 0;
    }

    @Override
    public int getBarTitleWithPath() {
        return 0;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        progressComp = (DataProgressBar) innerView.findViewById(R.id.progressComp);
        lvComp = (ListView) innerView.findViewById(R.id.lvComp);
        sv = (SearchView) innerView.findViewById(R.id.sv);

    }

    @Override
    public void initEvents() {

    }

    @Override
    public void initLogic() {

    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_autoboot;
    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
    }

    @Override
    public void initMenu(Menu menu) {

    }

    @Override
    public void onGetNewArguments(Bundle bn) {

    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }
}
