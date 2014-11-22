package com.rarnu.tools.root.fragment;

import android.app.AlertDialog;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.TextView;
import android.widget.Toast;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.BlockListView;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.AboutAdapter;
import com.rarnu.tools.root.adapter.PartnerAdapter;
import com.rarnu.tools.root.api.MobileApi;
import com.rarnu.tools.root.common.AboutInfo;
import com.rarnu.tools.root.fragmentactivity.BuildTeamActivity;
import com.rarnu.tools.root.utils.UpdateUtils;
import com.rarnu.utils.DeviceUtils;
import com.rarnu.utils.FileUtils;
import com.rarnu.utils.UIUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

public class AboutFragment extends BaseFragment implements OnItemClickListener {

    final Handler hUpdate = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                if (getActivity() != null) {
                    UpdateUtils.showUpdateInfo(getActivity(), false);
                }
            }
            super.handleMessage(msg);
        }
    };
    TextView tvAppVersion, tvDebug;
    BlockListView lstAbout, lstEoe;
    TextView tvAbout;
    AboutAdapter adapter = null;
    List<AboutInfo> list = null;
    PartnerAdapter adapterPartner = null;
    List<Integer> listPartner = null;
    TextView tvOfficial;
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
        tvOfficial = (TextView) innerView.findViewById(R.id.tvOfficial);

        lstAbout.setItemHeight(UIUtils.dipToPx(56));
        lstEoe.setItemHeight(UIUtils.dipToPx(64));

        list = new ArrayList<AboutInfo>();
        adapter = new AboutAdapter(getActivity(), list);
        lstAbout.setAdapter(adapter);
        listPartner = new ArrayList<Integer>();
        listPartner.add(0);
        listPartner.add(1);
        adapterPartner = new PartnerAdapter(getActivity(), listPartner);
        lstEoe.setAdapter(adapterPartner);
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
        list.add(buildAboutInfo(R.string.build_team, -1));
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
        tvOfficial.setVisibility(GlobalInstance.isOfficialVersion ? View.GONE : View.VISIBLE);
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

            Intent inWebsite = new Intent(Intent.ACTION_VIEW);
            switch (position) {
                case 0:
                    inWebsite.setData(Uri.parse("http://eoemarket.com/"));
                    break;
                case 1:
                    inWebsite.setData(Uri.parse("http://ucloud.cn/"));
                    break;
            }
            startActivity(inWebsite);
        } else {
            switch (position) {
                case 0:
                    Toast.makeText(getActivity(), R.string.checking_new_version, Toast.LENGTH_SHORT).show();
                    getUpdateInfo();
                    break;
                case 1:
                    // build team
                    Intent inBuildTeam = new Intent(getActivity(), BuildTeamActivity.class);
                    startActivity(inBuildTeam);
                    break;
                case 2:
                    fitableClick++;
                    if (fitableClick == 5) {
                        fitableClick = 0;
                        new AlertDialog.Builder(getActivity())
                                .setTitle(R.string.hint)
                                .setMessage(DeviceUtils.getDeviceUniqueId(getActivity()))
                                .setPositiveButton(R.string.ok, null)
                                .show();
                    }
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

    private void getUpdateInfo() {
        new Thread(new Runnable() {

            @Override
            public void run() {
                int verCode = DeviceUtils.getAppVersionCode(getActivity());
                String deviceId = DeviceUtils.getDeviceUniqueId(getActivity());
                GlobalInstance.updateInfo = MobileApi.checkUpdate(verCode, deviceId);
                hUpdate.sendEmptyMessage(1);
            }
        }).start();
    }
}
