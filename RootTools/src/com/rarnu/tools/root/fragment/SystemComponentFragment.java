package com.rarnu.tools.root.fragment;

import android.app.AlertDialog;
import android.content.DialogInterface;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.Toast;
import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.BlockListView;
import com.rarnu.devlib.component.DataProgressBar;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.BusyboxAdapter;
import com.rarnu.tools.root.common.BusyboxInfo;
import com.rarnu.tools.root.utils.DirHelper;
import com.rarnu.utils.FileUtils;
import com.rarnu.utils.UIUtils;

import java.util.ArrayList;
import java.util.List;

public class SystemComponentFragment extends BaseFragment implements OnItemClickListener {

    DataProgressBar progressBusybox;
    BlockListView lstBusybox;
    List<BusyboxInfo> list = null;
    BusyboxAdapter adapter = null;
    Handler hInstall = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                checkStatus();
                progressBusybox.setVisibility(View.GONE);
                lstBusybox.setEnabled(true);
            }
            super.handleMessage(msg);
        }
    };

    private void checkStatus() {
        boolean hasSu = RootUtils.hasSu();

        list.clear();
        list.add(buildBusyboxInfo(R.string.file_su, (hasSu ? BusyboxInfo.STATE_NORMAL : BusyboxInfo.STATE_BANNED)));
        list.add(buildBusyboxInfo(R.string.file_super_user, (RootUtils.hasSuperuser() ? BusyboxInfo.STATE_NORMAL : BusyboxInfo.STATE_WARNING)));
        list.add(buildBusyboxInfo(R.string.file_busybox, (RootUtils.hasBusybox() ? BusyboxInfo.STATE_NORMAL : BusyboxInfo.STATE_WARNING)));
        list.add(buildBusyboxInfo(R.string.file_iptables, (RootUtils.hasIptables() ? BusyboxInfo.STATE_NORMAL : BusyboxInfo.STATE_WARNING)));

        adapter.setNewList(list);
        lstBusybox.resize();
    }

    private BusyboxInfo buildBusyboxInfo(int resTitle, int state) {
        BusyboxInfo info = new BusyboxInfo();
        info.title = getString(resTitle);
        info.state = state;
        return info;
    }

    private void showSuStatus() {
        int ret = RootUtils.hasRoot();

        new AlertDialog.Builder(getActivity())
                .setTitle(R.string.hint)
                .setMessage((ret == 0 ? R.string.no_root_permission : R.string.has_su_file))
                .setPositiveButton(R.string.ok, null)
                .show();

    }

    @Override
    public int getBarTitle() {
        return R.string.busybox;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.busybox_with_path;
    }

    @Override
    public void initComponents() {
        lstBusybox = (BlockListView) innerView.findViewById(R.id.lstBusybox);
        progressBusybox = (DataProgressBar) innerView.findViewById(R.id.progressBusybox);
        lstBusybox.setItemHeight(UIUtils.dipToPx(56));

        list = new ArrayList<BusyboxInfo>();
        adapter = new BusyboxAdapter(getActivity(), list);
        lstBusybox.setAdapter(adapter);

    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_busybox;
    }

    @Override
    public void initMenu(Menu menu) {

    }

    @Override
    public void initLogic() {
        checkStatus();
    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        switch (position) {
            case 0:
                showSuStatus();
                break;
            case 2:
                // install busybox
                if (!RootUtils.hasBusybox()) {
                    doConfirmInstall(0);
                }
                break;
            case 3:
                // install iptables
                if (!RootUtils.hasIptables()) {
                    doConfirmInstall(1);
                }
                break;
        }
    }

    @Override
    public void initEvents() {
        lstBusybox.setOnItemClickListener(this);
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

    private void doConfirmInstall(final int mode) {
        new AlertDialog.Builder(getActivity())
                .setTitle(R.string.hint)
                .setMessage(mode == 0 ? R.string.install_busybox_confirm : R.string.install_iptables_confirm)
                .setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        doInstallFile(mode);
                    }
                })
                .setNegativeButton(R.string.cancel, null)
                .show();
    }

    private void doInstallFile(int mode) {
        boolean installBusybox = (mode == 0);
        boolean installIptables = (mode == 1);

        String abi = Build.CPU_ABI.toLowerCase();

        if (abi.startsWith("arm")) {
            if (installBusybox) {
                doInstallBusyboxT();
            }
            if (installIptables) {
                doInstallIptablesT();
            }
        } else {
            Toast.makeText(getActivity(), R.string.binary_not_supportted, Toast.LENGTH_LONG).show();
        }


    }

    private void doInstallBusyboxT() {

        String fileName = "busybox";
        doInstallT(fileName);
    }

    private void doInstallIptablesT() {

        String fileName = "iptables";
        String fileName6 = "ip6tables";
        doInstallT(fileName, fileName6);
    }

    private void doInstallT(final String... fileName) {

        progressBusybox.setVisibility(View.VISIBLE);
        progressBusybox.setAppName(getString(R.string.loading));
        lstBusybox.setEnabled(false);

        new Thread(new Runnable() {
            @Override
            public void run() {
                for (String fn : fileName) {
                    FileUtils.deleteFile(DirHelper.BUSYBOX_DIR + fn);
                    FileUtils.copyAssetFile(getActivity(), fn, DirHelper.BUSYBOX_DIR, null);

                    String cmd = String.format("cat %s%s > /system/xbin/%s", DirHelper.BUSYBOX_DIR, fn, fn);
                    CommandResult result = RootUtils.runCommand(cmd, true);
                    if (result.error.equals("")) {
                        RootUtils.runCommand("chmod 755 /system/xbin/" + fn, true);
                    }
                }
                hInstall.sendEmptyMessage(1);
            }
        }).start();
    }

}
