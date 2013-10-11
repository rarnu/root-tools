package com.rarnu.ucloud.android.fragment;

import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.ucloud.android.R;
import com.rarnu.ucloud.android.common.Config;
import com.rarnu.utils.ResourceUtils;

public class SettingsFragment extends BaseFragment implements CompoundButton.OnCheckedChangeListener, View.OnClickListener {

    CheckBox chkServerDown, chkDiskUsage, chkFlowUsage, chkCostUsage, chkServiceReceived, chkServiceRingtone, chkServiceVibrate, chkServiceLED;
    Button btnDiskUsage, btnFlowUsage, btnCostUsage, btnServiceRingtone;

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
        btnServiceRingtone = (Button) innerView.findViewById(R.id.btnServiceRingtone);
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
        btnServiceRingtone.setOnClickListener(this);
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
        btnFlowUsage.setText(String.format("%d%%", Config.getFLowPercent(getActivity())));
        btnCostUsage.setText(String.format("%d%%", Config.getCostPercent(getActivity())));
        btnServiceRingtone.setText(Config.getRingtoneUri(getActivity()).equals("") ? R.string.settings_service_ringtone_system : R.string.settings_service_ringtone_custom);

        btnDiskUsage.setEnabled(chkDiskUsage.isChecked());
        btnFlowUsage.setEnabled(chkFlowUsage.isChecked());
        btnCostUsage.setEnabled(chkCostUsage.isChecked());

        chkServiceRingtone.setEnabled(chkServiceReceived.isChecked());
        chkServiceVibrate.setEnabled(chkServiceReceived.isChecked());
        chkServiceLED.setEnabled(chkServiceReceived.isChecked());
        if (chkServiceReceived.isChecked()) {
            btnServiceRingtone.setEnabled(chkServiceRingtone.isChecked());
        } else {
            btnServiceRingtone.setEnabled(false);
        }
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
                if (isChecked) {
                    btnServiceRingtone.setEnabled(chkServiceRingtone.isChecked());
                } else {
                    btnServiceRingtone.setEnabled(false);
                }
                Config.setNotifyServiceReceived(getActivity(), isChecked);
                break;
            case R.id.chkServiceRingtone:
                btnServiceRingtone.setEnabled(isChecked);
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
        switch (v.getId()) {
            case R.id.btnDiskUsage:
                break;
            case R.id.btnFlowUsage:
                break;
            case R.id.btnCostUsage:
                break;
            case R.id.btnServiceRingtone:
                break;
        }
    }
}
