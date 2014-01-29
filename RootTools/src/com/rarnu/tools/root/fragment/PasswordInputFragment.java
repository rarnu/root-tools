package com.rarnu.tools.root.fragment;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.ImageView;
import android.widget.Toast;
import com.rarnu.devlib.base.BaseDialogFragment;
import com.rarnu.devlib.component.NumberPad;
import com.rarnu.devlib.component.PasswordView;
import com.rarnu.devlib.component.intf.NumberPadListener;
import com.rarnu.devlib.component.intf.OnPasswordInputListener;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;

public class PasswordInputFragment extends BaseDialogFragment implements View.OnClickListener, OnPasswordInputListener, NumberPadListener {

    ImageView btnSave;
    ImageView btnCancel;
    PasswordView etPassword;
    NumberPad npPwd;
    String password = "";

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
        etPassword = (PasswordView) innerView.findViewById(R.id.etPassword);
        npPwd = (NumberPad) innerView.findViewById(R.id.npPwd);
    }

    @Override
    public void initEvents() {
        btnSave.setOnClickListener(this);
        btnCancel.setOnClickListener(this);
        etPassword.setPasswordListener(this);
        npPwd.setNumberPadListener(this);
    }

    @Override
    public void initLogic() {
        password = "";
        etPassword.setPassword("");
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_password_input;
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
                if (password.length() > 4) {
                    password = password.substring(0, 4);
                }
                if (password.length() != 4) {
                    Toast.makeText(getActivity(), R.string.password_digit_number, Toast.LENGTH_SHORT).show();
                    return;
                }
                Intent inRet = new Intent();
                inRet.putExtra("password", password);
                getActivity().setResult(Activity.RESULT_OK, inRet);
                getActivity().finish();
                break;
            case R.id.btnCancel:
                getActivity().finish();
                break;
        }
    }

    @Override
    public void onPasswordInputed(String password) {

    }

    @Override
    public void onNumberClick(String number) {
        if (password.length() < 4) {
            password += number;
        }
        if (password.length() <= 4) {
            etPassword.setPassword(password);
        }
    }

    @Override
    public void onBackClick() {
        if (password.length() > 0) {
            password = password.substring(0, password.length() - 1);
            etPassword.setPassword(password);
        }
    }
}
