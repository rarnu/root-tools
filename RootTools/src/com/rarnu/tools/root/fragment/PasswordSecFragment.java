package com.rarnu.tools.root.fragment;

import android.app.Activity;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;
import com.rarnu.devlib.base.BaseDialogFragment;
import com.rarnu.devlib.component.NumberPad;
import com.rarnu.devlib.component.PasswordView;
import com.rarnu.devlib.component.intf.NumberPadListener;
import com.rarnu.devlib.component.intf.OnPasswordInputListener;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.database.PasswordCaller;

public class PasswordSecFragment extends BaseDialogFragment implements View.OnClickListener, OnPasswordInputListener, NumberPadListener {

    TextView tvFirstUse;
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
        tvFirstUse = (TextView) innerView.findViewById(R.id.tvFirstUse);
        btnCancel = (ImageView) innerView.findViewById(R.id.btnCancel);
        etPassword = (PasswordView) innerView.findViewById(R.id.etPassword);
        npPwd = (NumberPad) innerView.findViewById(R.id.npPwd);
    }

    @Override
    public void initEvents() {
        btnCancel.setOnClickListener(this);
        npPwd.setNumberPadListener(this);
        etPassword.setPasswordListener(this);
    }

    @Override
    public void initLogic() {
        password = "";
        etPassword.setPassword("");
        tvFirstUse.setVisibility(PasswordCaller.isInitSecPassword(getActivity()) ? View.VISIBLE : View.GONE);

    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_password_sec;
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
            case R.id.btnCancel:
                getActivity().finish();
                break;
        }
    }

    @Override
    public void onPasswordInputed(String password) {
        if (getActivity() != null) {
            if (PasswordCaller.isPasswordRight(getActivity(), password)) {
                getActivity().setResult(Activity.RESULT_OK);
                getActivity().finish();
            } else {
                Toast.makeText(getActivity(), R.string.password_wrong, Toast.LENGTH_SHORT).show();
                this.password = "";
                etPassword.setPassword("");
            }
        }
    }

    @Override
    public void onNumberClick(String number) {
        password += number;
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
