package com.rarnu.tools.root.fragment;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.Toast;
import com.rarnu.devlib.base.BasePopupFragment;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.database.PasswordCaller;
import com.rarnu.tools.root.fragmentactivity.PasswordInputActivity;

public class PasswordChangeSecFragment extends BasePopupFragment implements View.OnClickListener {

    Button btnOriPwd;
    Button btnNewPwd;
    Button btnRepeatPwd;
    MenuItem miSave;
    String pwdOri = "";
    String pwdNew = "";
    String pwdRepeat = "";

    @Override
    public int getBarTitle() {
        return R.string.change_sec_password;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.change_sec_password;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        btnOriPwd = (Button) innerView.findViewById(R.id.btnOriPwd);
        btnNewPwd = (Button) innerView.findViewById(R.id.btnNewPwd);
        btnRepeatPwd = (Button) innerView.findViewById(R.id.btnRepeatPwd);
    }

    @Override
    public void initEvents() {
        btnOriPwd.setOnClickListener(this);
        btnNewPwd.setOnClickListener(this);
        btnRepeatPwd.setOnClickListener(this);
    }

    @Override
    public void initLogic() {

    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_password_change_sec;
    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
    }

    @Override
    public void initMenu(Menu menu) {
        miSave = menu.add(0, MenuItemIds.MENU_SAVE, 99, R.string.save);
        miSave.setIcon(android.R.drawable.ic_menu_save);
        miSave.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuItemIds.MENU_SAVE:
                doUpdatePassword();
                break;
        }
        return true;
    }

    private void doUpdatePassword() {
        if (pwdNew.equals(pwdRepeat)) {
            boolean ret = PasswordCaller.changeSecPassword(getActivity(), pwdOri, pwdNew);
            if (ret) {
                Toast.makeText(getActivity(), R.string.password_change_succ, Toast.LENGTH_SHORT).show();
                getActivity().finish();
            } else {
                Toast.makeText(getActivity(), R.string.password_change_fail, Toast.LENGTH_SHORT).show();
            }
        } else {
            Toast.makeText(getActivity(), R.string.password_repeat_error, Toast.LENGTH_SHORT).show();
        }
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
        int reqId = -1;
        switch (v.getId()) {
            case R.id.btnOriPwd:
                reqId = 0;
                break;
            case R.id.btnNewPwd:
                reqId = 1;
                break;
            case R.id.btnRepeatPwd:
                reqId = 2;
                break;
        }
        startActivityForResult(new Intent(getActivity(), PasswordInputActivity.class), reqId);
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (resultCode != Activity.RESULT_OK) {
            return;
        }
        String pwd = data.getStringExtra("password");
        switch (requestCode) {
            case 0:
                pwdOri = pwd;
                if (pwdOri.equals("")) {
                    btnOriPwd.setText(R.string.password_click_input);
                } else {
                    btnOriPwd.setText("* * * *");
                }
                break;
            case 1:
                pwdNew = pwd;
                if (pwdNew.equals("")) {
                    btnNewPwd.setText(R.string.password_click_input);
                } else {
                    btnNewPwd.setText("* * * *");
                }
                break;
            case 2:
                pwdRepeat = pwd;
                if (pwdRepeat.equals("")) {
                    btnRepeatPwd.setText(R.string.password_click_input);
                } else {
                    btnRepeatPwd.setText("* * * *");
                }
                break;
        }
    }
}
