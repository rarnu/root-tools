package com.rarnu.tools.root.fragment;

import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.CheckBox;
import android.widget.ImageView;
import android.widget.TextView;
import com.rarnu.devlib.base.BaseDialogFragment;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.FileSystemFileInfo;
import com.rarnu.utils.FilePermissionUtils;
import com.rarnu.utils.common.FilePermissionInfo;

public class ChangePermissionFragment extends BaseDialogFragment implements View.OnClickListener {

    TextView tvHead;
    ImageView btnCancel, btnSave;
    FileSystemFileInfo item = null;
    FilePermissionInfo permission = null;
    CheckBox chkOwnerRead, chkOwnerWrite, chkOwnerExec;
    CheckBox chkGroupRead, chkGroupWrite, chkGroupExec;
    CheckBox chkOtherRead, chkOtherWrite, chkOtherExec;

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
        tvHead = (TextView) innerView.findViewById(R.id.tvHead);
        btnSave = (ImageView) innerView.findViewById(R.id.btnSave);
        btnCancel = (ImageView) innerView.findViewById(R.id.btnCancel);

        chkOwnerRead = (CheckBox) innerView.findViewById(R.id.chkOwnerRead);
        chkOwnerWrite = (CheckBox) innerView.findViewById(R.id.chkOwnerWrite);
        chkOwnerExec = (CheckBox) innerView.findViewById(R.id.chkOwnerExec);
        chkGroupRead = (CheckBox) innerView.findViewById(R.id.chkGroupRead);
        chkGroupWrite = (CheckBox) innerView.findViewById(R.id.chkGroupWrite);
        chkGroupExec = (CheckBox) innerView.findViewById(R.id.chkGroupExec);
        chkOtherRead = (CheckBox) innerView.findViewById(R.id.chkOtherRead);
        chkOtherWrite = (CheckBox) innerView.findViewById(R.id.chkOtherWrite);
        chkOtherExec = (CheckBox) innerView.findViewById(R.id.chkOtherExec);
    }

    @Override
    public void initEvents() {
        btnSave.setOnClickListener(this);
        btnCancel.setOnClickListener(this);
        chkOwnerRead.setOnClickListener(this);
        chkOwnerWrite.setOnClickListener(this);
        chkOwnerExec.setOnClickListener(this);
        chkGroupRead.setOnClickListener(this);
        chkGroupWrite.setOnClickListener(this);
        chkGroupExec.setOnClickListener(this);
        chkOtherRead.setOnClickListener(this);
        chkOtherWrite.setOnClickListener(this);
        chkOtherExec.setOnClickListener(this);
    }

    @Override
    public void initLogic() {
        item = (FileSystemFileInfo) getActivity().getIntent().getSerializableExtra("file");
        permission = FilePermissionUtils.getFilePermission(item.fullPath);
        tvHead.setText(item.name);
        if (permission != null) {
            chkOwnerRead.setChecked(permission.ownerRead);
            chkOwnerWrite.setChecked(permission.ownerWrite);
            chkOwnerExec.setChecked(permission.ownerExec);
            chkGroupRead.setChecked(permission.groupRead);
            chkGroupWrite.setChecked(permission.groupWrite);
            chkGroupExec.setChecked(permission.groupExec);
            chkOtherRead.setChecked(permission.otherRead);
            chkOtherWrite.setChecked(permission.otherWrite);
            chkOtherExec.setChecked(permission.otherExec);
        }
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_change_permission;
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

    @Override
    public void onClick(View v) {
        switch (v.getId()) {
            case R.id.btnSave:
                FilePermissionUtils.setFilePermission(item.fullPath, permission);
                getActivity().finish();
                break;
            case R.id.btnCancel:
                getActivity().finish();
                break;
            case R.id.chkOwnerRead:
                permission.ownerRead = chkOwnerRead.isChecked();
                break;
            case R.id.chkOwnerWrite:
                permission.ownerWrite = chkOwnerWrite.isChecked();
                break;
            case R.id.chkOwnerExec:
                permission.ownerExec = chkOwnerExec.isChecked();
                break;
            case R.id.chkGroupRead:
                permission.groupRead = chkGroupRead.isChecked();
                break;
            case R.id.chkGroupWrite:
                permission.groupWrite = chkGroupWrite.isChecked();
                break;
            case R.id.chkGroupExec:
                permission.groupExec = chkGroupExec.isChecked();
                break;
            case R.id.chkOtherRead:
                permission.otherRead = chkOtherRead.isChecked();
                break;
            case R.id.chkOtherWrite:
                permission.otherWrite = chkOtherWrite.isChecked();
                break;
            case R.id.chkOtherExec:
                permission.otherExec = chkOtherExec.isChecked();
                break;
        }

    }
}
