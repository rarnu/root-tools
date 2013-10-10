package com.rarnu.tools.root.fragment;

import android.app.Activity;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;
import com.rarnu.devlib.base.BaseDialogFragment;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.utils.MemorySpecialList;
import com.rarnu.tools.root.utils.MemoryUtils;

public class MemProcessFragment extends BaseDialogFragment implements OnClickListener {

    ImageView ImgIcon;
    TextView tvName, tvNamespace, tvWarning;
    TextView tvPidValue, tvMemoryValue, tvUserValue;
    Button btnKill, btnIgnore;
    ImageView btnCancel;

    @Override
    public void initComponents() {
        ImgIcon = (ImageView) innerView.findViewById(R.id.ImgIcon);
        tvName = (TextView) innerView.findViewById(R.id.tvName);
        tvNamespace = (TextView) innerView.findViewById(R.id.tvNamespace);
        tvWarning = (TextView) innerView.findViewById(R.id.tvWarning);
        tvPidValue = (TextView) innerView.findViewById(R.id.tvPidValue);
        tvMemoryValue = (TextView) innerView.findViewById(R.id.tvMemoryValue);
        tvUserValue = (TextView) innerView.findViewById(R.id.tvUserValue);
        btnCancel = (ImageView) innerView.findViewById(R.id.btnCancel);
        btnKill = (Button) innerView.findViewById(R.id.btnKill);
        btnIgnore = (Button) innerView.findViewById(R.id.btnIgnore);
    }

    @Override
    public void initLogic() {
        showProcessInfo();
        showIgnoreStatus();

    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_mem_process;
    }

    private void showIgnoreStatus() {
        int inIgnore = MemorySpecialList.inExcludeList(GlobalInstance.currentMemoryProcess.NAME);
        btnIgnore.setText(inIgnore != -1 ? R.string.remove_ignore : R.string.add_ignore);
    }

    private void showProcessInfo() {
        if (GlobalInstance.currentMemoryProcess.appInfo == null) {
            ImgIcon.setBackgroundDrawable(getResources().getDrawable(R.drawable.android));
            tvName.setText(GlobalInstance.currentMemoryProcess.NAME);
            tvNamespace.setText("");
        } else {
            ImgIcon.setBackgroundDrawable(GlobalInstance.pm.getApplicationIcon(GlobalInstance.currentMemoryProcess.appInfo));
            tvName.setText(GlobalInstance.pm.getApplicationLabel(GlobalInstance.currentMemoryProcess.appInfo));
            tvNamespace.setText(GlobalInstance.currentMemoryProcess.NAME);
        }

        tvPidValue.setText(String.valueOf(GlobalInstance.currentMemoryProcess.PID));
        tvMemoryValue.setText(String.format("%dM", GlobalInstance.currentMemoryProcess.RSS));
        tvUserValue.setText(GlobalInstance.currentMemoryProcess.USER);

    }

    @Override
    public void onClick(View v) {
        switch (v.getId()) {
            case R.id.btnKill:
                killProc();
                getActivity().setResult(Activity.RESULT_OK);
                getActivity().finish();
                break;
            case R.id.btnCancel:
                getActivity().finish();
                break;
            case R.id.btnIgnore:
                // add or remove ignore
                int inIgnore = MemorySpecialList.inExcludeList(GlobalInstance.currentMemoryProcess.NAME);
                if (inIgnore != -1) {
                    removeIgnore();
                } else {
                    addIgnore();
                }
                showIgnoreStatus();
                break;
        }

    }

    private void killProc() {
        if (MemorySpecialList.isExcludeLocked(GlobalInstance.currentMemoryProcess.NAME)) {
            Toast.makeText(getActivity(), R.string.locked_app_kill, Toast.LENGTH_LONG).show();
            return;
        }

        MemoryUtils.killProcess(GlobalInstance.currentMemoryProcess.PID);
    }

    private void addIgnore() {

        MemorySpecialList.addExclude(GlobalInstance.currentMemoryProcess.NAME);
        if (MemorySpecialList.saveExclude()) {
            Toast.makeText(getActivity(), R.string.added_ignore, Toast.LENGTH_LONG).show();
        } else {
            Toast.makeText(getActivity(), R.string.added_ignore_error, Toast.LENGTH_LONG).show();
        }
    }

    private void removeIgnore() {
        if (MemorySpecialList.isExcludeLocked(GlobalInstance.currentMemoryProcess.NAME)) {
            Toast.makeText(getActivity(), R.string.locked_app_error, Toast.LENGTH_LONG).show();
        } else {

            MemorySpecialList.removeExclude(GlobalInstance.currentMemoryProcess.NAME);
            if (MemorySpecialList.saveExclude()) {
                Toast.makeText(getActivity(), R.string.added_ignore, Toast.LENGTH_LONG).show();
            } else {
                Toast.makeText(getActivity(), R.string.added_ignore_error, Toast.LENGTH_LONG).show();
            }
        }
    }

    @Override
    public void initEvents() {
        btnCancel.setOnClickListener(this);
        btnKill.setOnClickListener(this);
        btnIgnore.setOnClickListener(this);
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

}
