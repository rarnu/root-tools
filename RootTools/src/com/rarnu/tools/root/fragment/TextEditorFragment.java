package com.rarnu.tools.root.fragment;

import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.EditText;
import android.widget.Toast;
import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;
import com.rarnu.devlib.base.BasePopupFragment;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.utils.DirHelper;
import com.rarnu.utils.FileUtils;

public class TextEditorFragment extends BasePopupFragment {

    EditText etEdit;
    MenuItem itemSave, itemRevert;
    String filePath = "";
    String fileName = "";

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
        etEdit = (EditText) innerView.findViewById(R.id.etEdit);

    }

    @Override
    public void initEvents() {
    }

    @Override
    public void initLogic() {
        filePath = getActivity().getIntent().getStringExtra("file");
        fileName = getActivity().getIntent().getStringExtra("name");
        getActivity().getActionBar().setTitle(fileName);
        try {
            String text = FileUtils.readFileString(filePath);
            etEdit.setText(text);
        } catch (Exception e) {

        }

    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_text_editor;
    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
    }

    @Override
    public void initMenu(Menu menu) {
        itemSave = menu.add(0, MenuItemIds.MENU_SAVE, 98, R.string.save);
        itemSave.setIcon(android.R.drawable.ic_menu_save);
        itemSave.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
        itemRevert = menu.add(0, MenuItemIds.MENU_REVERT, 99, R.string.revert);
        itemRevert.setIcon(android.R.drawable.ic_menu_revert);
        itemRevert.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuItemIds.MENU_SAVE:
                saveFile();
                break;
            case MenuItemIds.MENU_REVERT:
                try {
                    String text = FileUtils.readFileString(filePath);
                    etEdit.setText(text);
                } catch (Exception e) {

                }
                break;
        }
        return true;
    }

    private void saveFile() {
        try {
            String tmpFile = DirHelper.TEMP_DIR + "tmp";
            FileUtils.rewriteFile(tmpFile, etEdit.getText().toString());
            CommandResult cmdRet = RootUtils.runCommand(String.format("busybox cp \"%s\" \"%s\"", tmpFile, filePath), true);
            Toast.makeText(getActivity(), cmdRet.error.equals("") ? R.string.file_save_succ : R.string.file_save_fail, Toast.LENGTH_SHORT).show();

        } catch (Exception e) {

        }

    }

    @Override
    public void onGetNewArguments(Bundle bn) {
    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }
}
