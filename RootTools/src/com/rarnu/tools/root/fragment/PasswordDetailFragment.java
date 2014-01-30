package com.rarnu.tools.root.fragment;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.EditText;
import android.widget.Toast;
import com.rarnu.devlib.base.BasePopupFragment;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.common.PasswordItem;
import com.rarnu.tools.root.database.PasswordCaller;
import com.rarnu.utils.InputMethodUtils;

public class PasswordDetailFragment extends BasePopupFragment {
    boolean isAdd = false;
    boolean isEdit = false;
    PasswordItem passwordItem = null;

    MenuItem miSave, miEdit, miDelete, miCancel;

    EditText etName, etAccount, etPassword, etMemo;

    @Override
    public int getBarTitle() {
        return R.string.pdetail_title;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.pdetail_title;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        etName = (EditText) innerView.findViewById(R.id.etName);
        etAccount = (EditText) innerView.findViewById(R.id.etAccount);
        etPassword = (EditText) innerView.findViewById(R.id.etPassword);
        etMemo = (EditText) innerView.findViewById(R.id.etMemo);
    }

    @Override
    public void initEvents() {

    }

    @Override
    public void initLogic() {
        isAdd = getActivity().getIntent().getBooleanExtra("isAdd", false);
        isEdit = isAdd;
        if (!isEdit) {
            passwordItem = (PasswordItem) getActivity().getIntent().getSerializableExtra("item");
            reloadData();
            setEditMode(false);
        } else {
            setEditMode(true);
        }
    }

    private void reloadData() {
        etName.setText(passwordItem.name);
        etAccount.setText(passwordItem.account);
        etPassword.setText(passwordItem.password);
        etMemo.setText(passwordItem.memo);
    }

    private void setEditMode(boolean edit) {
        isEdit = edit;
        setEditable(etName, edit);
        setEditable(etAccount, edit);
        setEditable(etPassword, edit);
        setEditable(etMemo, edit);
        if (miSave != null) {
            miSave.setVisible(edit);
        }
        if (miCancel != null) {
            miCancel.setVisible(edit);
        }
        if (miEdit != null) {
            miEdit.setVisible(!edit);
        }
        if (miDelete != null) {
            miDelete.setVisible(!edit);
        }

        if (edit) {
            etName.requestFocus();
        }
    }

    private void setEditable(EditText et, boolean editable) {
        et.setCursorVisible(editable);
        et.setFocusable(editable);
        et.setFocusableInTouchMode(editable);
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_password_detail;
    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
    }

    @Override
    public void initMenu(Menu menu) {
        miSave = menu.add(0, MenuItemIds.MENU_SAVE, 98, R.string.save);
        miSave.setIcon(android.R.drawable.ic_menu_save);
        miSave.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
        miCancel = menu.add(0, MenuItemIds.MENU_CANCEL, 99, R.string.cancel);
        miCancel.setIcon(android.R.drawable.ic_menu_close_clear_cancel);
        miCancel.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
        miEdit = menu.add(0, MenuItemIds.MENU_EDIT, 98, R.string.edit);
        miEdit.setIcon(android.R.drawable.ic_menu_edit);
        miEdit.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
        miDelete = menu.add(0, MenuItemIds.MENU_DELETE, 99, R.string.delete);
        miDelete.setIcon(android.R.drawable.ic_menu_delete);
        miDelete.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
        setEditMode(isEdit);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuItemIds.MENU_SAVE:
                doSave();
                break;
            case MenuItemIds.MENU_CANCEL:
                doCancel();
                break;
            case MenuItemIds.MENU_EDIT:
                setEditMode(true);
                break;
            case MenuItemIds.MENU_DELETE:
                doDelete();
                break;
        }
        return true;
    }

    @Override
    public void onGetNewArguments(Bundle bn) {

    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

    private void doSave() {
        String name = etName.getText().toString();
        if (name.equals("")) {
            Toast.makeText(getActivity(), R.string.detail_name_empty, Toast.LENGTH_SHORT).show();
            return;
        }
        String account = etAccount.getText().toString();
        String password = etPassword.getText().toString();
        String memo = etMemo.getText().toString();
        boolean ret = false;
        if (isAdd) {
            ret = PasswordCaller.addPassword(getActivity(), name, account, password, memo);
        } else {
            ret = PasswordCaller.updatePassword(getActivity(), passwordItem.id, name, account, password, memo);
        }
        if (ret) {
            if (isAdd) {
                returnReload(true);
            } else {
                passwordItem.name = name;
                passwordItem.account = account;
                passwordItem.password = password;
                passwordItem.memo = memo;
                reloadData();
                InputMethodUtils.hideInputMethod(getActivity());
                setEditMode(false);
                returnReload(false);
            }
        } else {
            Toast.makeText(getActivity(), R.string.detail_save_failed, Toast.LENGTH_SHORT).show();
        }
    }

    private void doCancel() {
        new AlertDialog.Builder(getActivity())
                .setTitle(R.string.hint)
                .setMessage(R.string.detail_cancel_hint)
                .setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        if (isAdd) {
                            getActivity().finish();
                        } else {
                            reloadData();
                            InputMethodUtils.hideInputMethod(getActivity());
                            setEditMode(false);
                        }
                    }
                })
                .setNegativeButton(R.string.cancel, null)
                .show();

    }

    private void doDelete() {
        new AlertDialog.Builder(getActivity())
                .setTitle(R.string.hint)
                .setMessage(R.string.detail_delete_hint)
                .setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        boolean ret = PasswordCaller.deletePassword(getActivity(), passwordItem.id);
                        if (ret) {
                            returnReload(true);
                        } else {
                            Toast.makeText(getActivity(), R.string.detail_delete_failed, Toast.LENGTH_SHORT).show();
                        }
                    }
                })
                .setNegativeButton(R.string.cancel, null)
                .show();

    }

    private void returnReload(boolean needFinish) {
        Intent inRet = new Intent();
        inRet.putExtra("reload", true);
        getActivity().setResult(Activity.RESULT_OK, inRet);
        if (needFinish) {
            getActivity().finish();
        }
    }
}
