package com.yugioh.android.fragments;

import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.view.Menu;
import android.widget.TextView;
import com.rarnu.devlib.base.BaseDialogFragment;
import com.rarnu.utils.DeviceUtils;
import com.rarnu.utils.ResourceUtils;
import com.yugioh.android.R;

public class AboutFragment extends BaseDialogFragment {

    TextView tvVersion, tvAboutDate;

    public AboutFragment() {
        super();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.fragment_about;
    }

    @Override
    public void initComponents() {
        tvVersion = (TextView) innerView.findViewById(R.id.tvVersion);
        tvAboutDate = (TextView) innerView.findViewById(R.id.tvAboutDate);
    }

    @Override
    public void initEvents() {

    }

    @Override
    public void initLogic() {
        tvVersion.setText(DeviceUtils.getAppVersionName(getActivity()));
        String releaseDate = "";
        try {
            ApplicationInfo appInfo = getActivity().getPackageManager().getApplicationInfo(getActivity().getPackageName(), PackageManager.GET_META_DATA);
            releaseDate = appInfo.metaData.getString("release-date");

        } catch (Exception e) {
        }
        tvAboutDate.setText(getString(R.string.about_date_fmt, releaseDate));
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
    public String getMainActivityName() {
        return null;
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
