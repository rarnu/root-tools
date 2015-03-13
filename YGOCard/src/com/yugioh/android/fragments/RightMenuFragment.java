package com.yugioh.android.fragments;

import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ListView;
import android.widget.RelativeLayout;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.UIUtils;
import com.yugioh.android.*;
import com.yugioh.android.adapter.RightMenuAdapter;
import com.yugioh.android.classes.RightMenuItem;
import com.yugioh.android.classes.UpdateInfo;

import java.util.ArrayList;
import java.util.List;

public class RightMenuFragment extends BaseFragment implements OnItemClickListener {

    ListView lvSettings;
    List<RightMenuItem> listSettings;
    RightMenuAdapter adapterSettings;
    UpdateInfo updateInfo;

    public RightMenuFragment() {
        super();
    }

    @Override
    public int getBarTitle() {
        return R.string.app_name;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.app_name;
    }

    @Override
    public void initComponents() {

        lvSettings = (ListView) innerView.findViewById(R.id.lvSettings);

        listSettings = new ArrayList<RightMenuItem>();
        RightMenuItem itemSettings = new RightMenuItem(getString(R.string.rm_settings));
        RightMenuItem itemUpdate = new RightMenuItem(getString(R.string.rm_update));
        RightMenuItem itemFeedback = new RightMenuItem(getString(R.string.rm_feedback));
        RightMenuItem itemAbout = new RightMenuItem(getString(R.string.rm_about));
        listSettings.add(itemSettings);
        listSettings.add(itemUpdate);
        listSettings.add(itemFeedback);
        listSettings.add(itemAbout);
        adapterSettings = new RightMenuAdapter(getActivity(), listSettings);
        lvSettings.setAdapter(adapterSettings);

        int lvHeight = UIUtils.dipToPx((int) (48 + UIUtils.getDensity() * 2) * 4);
        int marginTop = (UIUtils.getHeight() - UIUtils.getStatusBarHeight() - lvHeight) / 2;
        RelativeLayout.LayoutParams rllp = (RelativeLayout.LayoutParams) lvSettings.getLayoutParams();
        rllp.topMargin = marginTop;
        rllp.height = lvHeight;
        lvSettings.setLayoutParams(rllp);
    }

    @Override
    public void initEvents() {
        lvSettings.setOnItemClickListener(this);
    }

    @Override
    public void initLogic() {

    }

    @Override
    public void onResume() {
        super.onResume();

    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.menu_right;
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
    public void onItemClick(AdapterView<?> parent, View view, int position,
                            long id) {
        switch (parent.getId()) {
            case R.id.lvSettings:
                switch (position) {
                    case 0:
                        startActivity(new Intent(getActivity(), SettingsActivity.class));
                        break;
                    case 1:
                        startActivity(new Intent(getActivity(), UpdateActivity.class));
                        break;
                    case 2:
                        startActivity(new Intent(getActivity(), FeedbackActivity.class));
                        break;
                    case 3:
                        startActivity(new Intent(getActivity(), AboutActivity.class));
                        break;
                }
                break;

        }
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
