package com.rarnu.ucloud.android.fragment;

import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.EditText;
import android.widget.ImageView;
import com.rarnu.devlib.base.BaseDialogFragment;
import com.rarnu.ucloud.android.R;

public class LoginFragment extends BaseDialogFragment implements View.OnClickListener {
    ImageView btnSave;
    ImageView btnCancel;
    EditText etUserName;
    EditText etPassword;

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
        btnSave = (ImageView) innerView.findViewById(R.id.btnSave);
        btnCancel = (ImageView) innerView.findViewById(R.id.btnCancel);
        etUserName = (EditText) innerView.findViewById(R.id.etUserName);
        etPassword = (EditText) innerView.findViewById(R.id.etPassword);
    }

    @Override
    public void initEvents() {
        btnSave.setOnClickListener(this);
        btnCancel.setOnClickListener(this);
    }

    @Override
    public void initLogic() {

    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.dialog_login;
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
    public void onClick(View v) {
        switch (v.getId()) {
            case R.id.btnSave:
                // TODO: login
                getActivity().finish();
                break;
            case R.id.btnCancel:
                getActivity().finish();
                break;
        }
    }
}
