package com.rarnu.tools.root.fragment;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager.NameNotFoundException;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.*;
import com.rarnu.devlib.base.BasePopupFragment;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.SysappInfo;
import com.rarnu.tools.root.utils.ApkUtils;
import com.rarnu.tools.root.utils.CustomPackageUtils;

import java.io.File;

public class SysappDetailFragment extends BasePopupFragment implements OnClickListener {

    ImageView appIcon;
    TextView appName;
    TextView appPath;
    TextView appVersion;
    Button btnDelete;
    Button btnAddToCleanList;
    TextView tvPathDetail;
    TextView tvOdexDetail;
    TextView tvFileSizeDetail;
    TextView tvDataPathDetail;
    TextView tvSharedIdDetail;
    TextView tvDataSizeDetail;
    SysappInfo info = null;
    PackageInfo pinfo = null;

    @Override
    public int getBarTitle() {
        return R.string.sysapp_name;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.sysapp_name;
    }

    public void showAppInfo() {
        info = GlobalInstance.currentSysapp;
        try {
            pinfo = GlobalInstance.pm.getPackageInfo(info.info.packageName, Context.MODE_APPEND);
        } catch (NameNotFoundException e) {
            pinfo = null;
        }
        if (pinfo == null) {
            getActivity().finish();
            return;
        }

        appIcon.setBackgroundDrawable(GlobalInstance.pm.getApplicationIcon(info.info));
        appName.setText(GlobalInstance.pm.getApplicationLabel(info.info));
        appVersion.setText(getResources().getString(R.string.version) + (pinfo == null ? getResources().getString(R.string.unknown) : pinfo.versionName));
        tvPathDetail.setText(info.info.sourceDir.replace("/system/app/", ""));
        String odexPath = info.info.sourceDir.substring(0, info.info.sourceDir.length() - 3) + "odex";
        File fOdex = new File(odexPath);
        tvOdexDetail.setText(fOdex.exists() ? odexPath.replace("/system/app/", "") : getResources().getString(R.string.na));
        tvFileSizeDetail.setText(ApkUtils.getAppSize(info.info.sourceDir) + " KB " + String.format("(%s)", fOdex.exists() ? "APK+ODEX" : "APK"));
        tvDataPathDetail.setText(info.info.dataDir.replace("/data/data/", ""));
        String dataSize = ApkUtils.getDataSize(info.info.dataDir);
        tvDataSizeDetail.setText(dataSize.equals("") ? getResources().getString(R.string.unknown) : dataSize + " KB");
        String sid = pinfo.sharedUserId;
        if (sid == null) {
            sid = "";
        }
        sid = sid.trim();
        tvSharedIdDetail.setText(sid.equals("") ? getResources().getString(R.string.na) : sid);
        if (!GlobalInstance.allowDeleteLevel0) {
            if (info.level == 0) {
                RelativeLayout.LayoutParams rlp = (RelativeLayout.LayoutParams) btnDelete.getLayoutParams();
                rlp.height = 0;
                btnDelete.setLayoutParams(rlp);
                btnDelete.setEnabled(false);
            }
        }
        btnAddToCleanList.setText(CustomPackageUtils.customPackageIndex(info.info.packageName) == -1 ? R.string.button_add_to_clean_list : R.string.button_remove_from_clean_list);
    }

    @Override
    public void onClick(View v) {
        switch (v.getId()) {
            case R.id.btnAddToCleanList:
                addToCleanList();
                break;
            case R.id.btnDelete:
                confirmDelete();
                break;
        }

    }

    private void addToCleanList() {
        if (CustomPackageUtils.customPackageIndex(info.info.packageName) == -1) {
            CustomPackageUtils.addCustomPackage(appName.getText().toString(), info.info.packageName);
        } else {
            CustomPackageUtils.removeCustomPackage(info.info.packageName);
        }
        CustomPackageUtils.saveCustomPackages();
        btnAddToCleanList.setText(CustomPackageUtils.customPackageIndex(info.info.packageName) == -1 ? R.string.button_add_to_clean_list : R.string.button_remove_from_clean_list);
    }

    private void confirmDelete() {
        String hintStr = "";
        switch (info.level) {
            case 0:
                hintStr = getResources().getString(R.string.delete_android_app);
                break;
            case 1:
                hintStr = getResources().getString(R.string.delete_google_app);
                break;
            case 2:
                hintStr = getResources().getString(R.string.delete_htc_app);
                break;
            case 3:
                hintStr = getResources().getString(R.string.delete_system_app);
                break;
        }

        // delete system app
        new AlertDialog.Builder(getActivity())
                .setTitle(R.string.hint)
                .setMessage(hintStr)
                .setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        deleteApp(GlobalInstance.backupBeforeDelete, GlobalInstance.alsoDeleteData);
                    }
                })
                .setNegativeButton(R.string.cancel, null)
                .show();
    }

    private void deleteApp(boolean backup, boolean deleteData) {
        if (backup) {
            ApkUtils.backupSystemApp(info.info.sourceDir);
        }

        boolean ret = ApkUtils.deleteSystemApp(info.info.sourceDir);
        if (!ret) {
            Toast.makeText(getActivity(), R.string.delete_fail, Toast.LENGTH_LONG).show();
            return;
        }
        if (deleteData) {
            ApkUtils.deleteSystemAppData(info.info.dataDir);
        }
        Intent inRet = new Intent();
        inRet.putExtra("needRefresh", true);
        getActivity().setResult(Activity.RESULT_OK, inRet);
        getActivity().finish();
    }

    @Override
    public void initComponents() {
        appIcon = (ImageView) innerView.findViewById(R.id.appIcon);
        appName = (TextView) innerView.findViewById(R.id.appName);
        appVersion = (TextView) innerView.findViewById(R.id.appVersion);
        btnDelete = (Button) innerView.findViewById(R.id.btnDelete);
        btnAddToCleanList = (Button) innerView.findViewById(R.id.btnAddToCleanList);

        tvPathDetail = (TextView) innerView.findViewById(R.id.tvPathDetail);
        tvOdexDetail = (TextView) innerView.findViewById(R.id.tvOdexDetail);
        tvFileSizeDetail = (TextView) innerView.findViewById(R.id.tvFileSizeDetail);
        tvDataPathDetail = (TextView) innerView.findViewById(R.id.tvDataPathDetail);
        tvSharedIdDetail = (TextView) innerView.findViewById(R.id.tvSharedIdDetail);
        tvDataSizeDetail = (TextView) innerView.findViewById(R.id.tvDataSizeDetail);

    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_sysapp_detail;
    }

    @Override
    public void initMenu(Menu menu) {

    }

    @Override
    public void initLogic() {
        showAppInfo();
    }

    @Override
    public void initEvents() {
        btnDelete.setOnClickListener(this);
        btnAddToCleanList.setOnClickListener(this);

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
