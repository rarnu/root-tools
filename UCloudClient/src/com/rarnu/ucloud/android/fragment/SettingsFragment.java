package com.rarnu.ucloud.android.fragment;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.ucloud.android.R;
import com.rarnu.ucloud.android.UserActivity;
import com.rarnu.ucloud.android.common.Config;
import com.rarnu.ucloud.android.dialog.LoginDialog;
import com.rarnu.ucloud.android.dialog.SeekDialog;
import com.rarnu.utils.ResourceUtils;

public class SettingsFragment extends BaseFragment implements CompoundButton.OnCheckedChangeListener, View.OnClickListener {

    CheckBox chkServerDown, chkDiskUsage, chkFlowUsage, chkCostUsage, chkServiceReceived, chkServiceRingtone, chkServiceVibrate, chkServiceLED;
    Button btnDiskUsage, btnFlowUsage, btnCostUsage;
    Button btnLogin, btnAccountInfo;

    public SettingsFragment() {
        super();
        tagText = ResourceUtils.getString(R.string.tag_settings_fragment);
        tabTitle = ResourceUtils.getString(R.string.title_settings_fragment);
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
        chkServerDown = (CheckBox) innerView.findViewById(R.id.chkServerDown);
        chkDiskUsage = (CheckBox) innerView.findViewById(R.id.chkDiskUsage);
        chkFlowUsage = (CheckBox) innerView.findViewById(R.id.chkFlowUsage);
        chkCostUsage = (CheckBox) innerView.findViewById(R.id.chkCostUsage);
        chkServiceReceived = (CheckBox) innerView.findViewById(R.id.chkServiceReceived);
        chkServiceRingtone = (CheckBox) innerView.findViewById(R.id.chkServiceRingtone);
        chkServiceVibrate = (CheckBox) innerView.findViewById(R.id.chkServiceVibrate);
        chkServiceLED = (CheckBox) innerView.findViewById(R.id.chkServiceLED);

        btnDiskUsage = (Button) innerView.findViewById(R.id.btnDiskUsage);
        btnFlowUsage = (Button) innerView.findViewById(R.id.btnFlowUsage);
        btnCostUsage = (Button) innerView.findViewById(R.id.btnCostUsage);

        btnLogin = (Button) innerView.findViewById(R.id.btnLogin);
        btnAccountInfo = (Button) innerView.findViewById(R.id.btnAccountInfo);
    }

    @Override
    public void initEvents() {
        chkServerDown.setOnCheckedChangeListener(this);
        chkDiskUsage.setOnCheckedChangeListener(this);
        chkFlowUsage.setOnCheckedChangeListener(this);
        chkCostUsage.setOnCheckedChangeListener(this);
        chkServiceReceived.setOnCheckedChangeListener(this);
        chkServiceRingtone.setOnCheckedChangeListener(this);
        chkServiceVibrate.setOnCheckedChangeListener(this);
        chkServiceLED.setOnCheckedChangeListener(this);
        btnDiskUsage.setOnClickListener(this);
        btnFlowUsage.setOnClickListener(this);
        btnCostUsage.setOnClickListener(this);
        btnLogin.setOnClickListener(this);
        btnAccountInfo.setOnClickListener(this);
    }

    @Override
    public void initLogic() {
        loadConfig();
    }

    private void loadConfig() {
        chkServerDown.setChecked(Config.getNotifyServerDown(getActivity()));
        chkDiskUsage.setChecked(Config.getNotifyDiskUsage(getActivity()));
        chkFlowUsage.setChecked(Config.getNotifyFlowUsage(getActivity()));
        chkCostUsage.setChecked(Config.getNotifyCostUsage(getActivity()));
        chkServiceReceived.setChecked(Config.getNotifyServiceReceived(getActivity()));
        chkServiceRingtone.setChecked(Config.getServiceRingtone(getActivity()));
        chkServiceVibrate.setChecked(Config.getServiceVibrate(getActivity()));
        chkServiceLED.setChecked(Config.getServiceLed(getActivity()));

        btnDiskUsage.setText(String.format("%d%%", Config.getDiskPercent(getActivity())));
        btnFlowUsage.setText(String.format("%d%%", Config.getFlowPercent(getActivity())));
        btnCostUsage.setText(String.format("%d%%", Config.getCostPercent(getActivity())));

        btnDiskUsage.setEnabled(chkDiskUsage.isChecked());
        btnFlowUsage.setEnabled(chkFlowUsage.isChecked());
        btnCostUsage.setEnabled(chkCostUsage.isChecked());

        chkServiceRingtone.setEnabled(chkServiceReceived.isChecked());
        chkServiceVibrate.setEnabled(chkServiceReceived.isChecked());
        chkServiceLED.setEnabled(chkServiceReceived.isChecked());
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.fragment_settings;
    }

    @Override
    public String getMainActivityName() {
        return "";
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

    @Override
    public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
        switch (buttonView.getId()) {
            case R.id.chkServerDown:
                Config.setNotifyServerDown(getActivity(), isChecked);
                break;
            case R.id.chkDiskUsage:
                btnDiskUsage.setEnabled(isChecked);
                Config.setNotifyDiskUsage(getActivity(), isChecked);
                break;
            case R.id.chkFlowUsage:
                btnFlowUsage.setEnabled(isChecked);
                Config.setNotifyFlowUsage(getActivity(), isChecked);
                break;
            case R.id.chkCostUsage:
                btnCostUsage.setEnabled(isChecked);
                Config.setNotifyCostUsage(getActivity(), isChecked);
                break;
            case R.id.chkServiceReceived:
                chkServiceRingtone.setEnabled(isChecked);
                chkServiceVibrate.setEnabled(isChecked);
                chkServiceLED.setEnabled(isChecked);
                Config.setNotifyServiceReceived(getActivity(), isChecked);
                break;
            case R.id.chkServiceRingtone:
                Config.setServiceRingtone(getActivity(), isChecked);
                break;
            case R.id.chkServiceVibrate:
                Config.setServiceVibrate(getActivity(), isChecked);
                break;
            case R.id.chkServiceLED:
                Config.setServiceLed(getActivity(), isChecked);
                break;
        }
    }

    @Override
    public void onClick(View v) {

        Intent inSeek = new Intent(getActivity(), SeekDialog.class);

        switch (v.getId()) {
            case R.id.btnDiskUsage:
                inSeek.putExtra("title", getString(R.string.settings_disk_usage));
                inSeek.putExtra("progress", Config.getDiskPercent(getActivity()));
                startActivityForResult(inSeek, 0);
                break;
            case R.id.btnFlowUsage:
                inSeek.putExtra("title", getString(R.string.settings_flow_usage));
                inSeek.putExtra("progress", Config.getFlowPercent(getActivity()));
                startActivityForResult(inSeek, 1);
                break;
            case R.id.btnCostUsage:
                inSeek.putExtra("title", getString(R.string.settings_cost_usage));
                inSeek.putExtra("progress", Config.getCostPercent(getActivity()));
                startActivityForResult(inSeek, 2);
                break;
            case R.id.btnLogin:
                startActivityForResult(new Intent(getActivity(), LoginDialog.class), 3);
                break;
            case R.id.btnAccountInfo:
                startActivity(new Intent(getActivity(), UserActivity.class));
                break;
        }
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (resultCode != Activity.RESULT_OK) {
            return;
        }

        switch (requestCode) {
            case 0:
                Config.setDiskPercent(getActivity(), data.getIntExtra("progress", 0));
                btnDiskUsage.setText(String.format("%d%%", Config.getDiskPercent(getActivity())));
                break;
            case 1:
                Config.setFlowPercent(getActivity(), data.getIntExtra("progress", 0));
                btnFlowUsage.setText(String.format("%d%%", Config.getFlowPercent(getActivity())));
                break;
            case 2:
                Config.setCostPercent(getActivity(), data.getIntExtra("progress", 0));
                btnCostUsage.setText(String.format("%d%%", Config.getCostPercent(getActivity())));
                break;
        }
    }
}
