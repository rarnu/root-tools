package com.rarnu.tools.root.fragment;

import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.TextView;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.BlockListView;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.AboutAdapter;
import com.rarnu.tools.root.adapter.PartnerAdapter;
import com.rarnu.tools.root.common.AboutInfo;
import com.rarnu.tools.root.utils.DeviceUtils;
import com.rarnu.tools.root.utils.UpdateUtils;
import com.rarnu.utils.FileUtils;
import com.rarnu.utils.UIUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

public class AboutFragment extends BaseFragment implements OnItemClickListener {

    TextView tvAppVersion, tvDebug;
    BlockListView lstAbout, lstEoe;
    TextView tvAbout;
    AboutAdapter adapter = null;
    List<AboutInfo> list = null;
    PartnerAdapter adapterEoe = null;
    List<Object> listEoe = null;
    int fitable = 5;
    int fitableClick = 0;

    private void showDebugStatus() {
        tvDebug.setVisibility(GlobalInstance.DEBUG ? View.VISIBLE : View.GONE);
    }

    private void showAppVersion() {
        tvAppVersion.setText(DeviceUtils.getAppVersionName(getActivity()));
    }

    private int getSystemFitable() {

        fitable = DeviceUtils.getFitable(UIUtils.getDM());
        if (fitable < 1) {
            fitable = 1;
        }
        if (fitable > 9) {
            fitable = 9;
        }
        return fitable;
    }

    @Override
    public int getBarTitle() {
        return R.string.about;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.about_with_path;
    }

    @Override
    public void initComponents() {
        tvAppVersion = (TextView) innerView.findViewById(R.id.tvAppVersion);
        tvDebug = (TextView) innerView.findViewById(R.id.tvDebug);
        lstAbout = (BlockListView) innerView.findViewById(R.id.lstAbout);
        lstEoe = (BlockListView) innerView.findViewById(R.id.lstEoe);
        tvAbout = (TextView) innerView.findViewById(R.id.tvAbout);

        lstAbout.setItemHeight(UIUtils.dipToPx(56));
        lstEoe.setItemHeight(UIUtils.dipToPx(64));

        list = new ArrayList<AboutInfo>();
        adapter = new AboutAdapter(getActivity(), list);
        lstAbout.setAdapter(adapter);
        listEoe = new ArrayList<Object>();
        listEoe.add(new Object());
        adapterEoe = new PartnerAdapter(getActivity(), listEoe);
        lstEoe.setAdapter(adapterEoe);
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_about;
    }

    @Override
    public void initMenu(Menu menu) {

    }

    @Override
    public void initLogic() {
        showAppVersion();
        showDebugStatus();
        fitableClick = 0;

        list.clear();
        list.add(buildAboutInfo(R.string.check_update, -1));
        list.add(buildAboutInfo(R.string.how_to_use, -1));
        list.add(buildAboutInfo(R.string.system_fitable, getSystemFitable()));

        adapter.setNewList(list);
        lstAbout.resize();
        lstEoe.resize();

        String lang = Locale.getDefault().getLanguage();
        String country = Locale.getDefault().getCountry();

        String aboutText = "";
        try {
            if (lang.equals("zh")) {
                aboutText = FileUtils.readAssetFile(getActivity(), country.equals("TW") ? "about_zh_TW" : "about_zh_CN");
            } else {
                aboutText = FileUtils.readAssetFile(getActivity(), "about");
            }
        } catch (Exception e) {

        }
        tvAbout.setText(aboutText);
    }

    private AboutInfo buildAboutInfo(int resTitle, int fitable) {
        AboutInfo info = new AboutInfo();
        info.title = getString(resTitle);
        info.fitable = fitable;
        return info;
    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        if (parent.getAdapter() instanceof PartnerAdapter) {
            Intent inEoe = new Intent(Intent.ACTION_VIEW);
            inEoe.setData(Uri.parse("http://eoemarket.com/"));
            startActivity(inEoe);
        } else {
            switch (position) {
                case 0:
                    UpdateUtils.showUpdateInfo(getActivity());
                    break;
            }
        }
    }

    @Override
    public void initEvents() {
        lstAbout.setOnItemClickListener(this);
        lstEoe.setOnItemClickListener(this);

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
