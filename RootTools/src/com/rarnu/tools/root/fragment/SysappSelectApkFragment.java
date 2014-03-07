package com.rarnu.tools.root.fragment;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.os.Environment;
import android.view.Menu;
import android.view.MenuItem;
import com.rarnu.devlib.base.BasePopupFragment;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.component.FileExploreListener;
import com.rarnu.tools.root.component.FileExplorerView;
import com.rarnu.utils.common.FileSystemFileInfo;

import java.io.File;

public class SysappSelectApkFragment extends BasePopupFragment implements FileExploreListener {

    private static String rootDir = Environment.getExternalStorageDirectory().getAbsolutePath();

    FileExplorerView fevFiles;
    MenuItem itemUp;

    @Override
    public int getBarTitle() {
        return R.string.sysapp_select;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.sysapp_select;
    }

    @Override
    public void initComponents() {
        fevFiles = (FileExplorerView) innerView.findViewById(R.id.fevFiles);
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_sysapp_selectapk;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuItemIds.MENU_UPLEVEL:
                fevFiles.doUpLevel();
                break;
        }
        return true;
    }

    @Override
    public void initMenu(Menu menu) {
        itemUp = menu.add(0, MenuItemIds.MENU_UPLEVEL, 98, R.string.uplevel);
        itemUp.setIcon(R.drawable.up_level);
        itemUp.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
    }

    @Override
    public void initLogic() {
        fevFiles.setFileType(".apk");
        File fSdCard = new File("/sdcard");
        if (fSdCard.exists()) {
            fevFiles.setCurrentDir("/sdcard");
        } else {
            fevFiles.setCurrentDir(rootDir);
        }
    }

    @Override
    public void initEvents() {
        fevFiles.setFileExploreListener(this);
    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
    }

    @Override
    public void onGetNewArguments(Bundle bn) {
        fevFiles.doUpLevel();
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

    @Override
    public void onGotoStart() {
        if (itemUp != null) {
            itemUp.setEnabled(false);
        }
    }

    @Override
    public void onGotoEnd() {
        if (itemUp != null) {
            itemUp.setEnabled(true);
        }
    }

    @Override
    public void onFileItemLongClick(FileSystemFileInfo item) {
        if (!item.fullPath.toLowerCase().endsWith(".apk")) {
            return;
        }

        File f = new File(item.fullPath);
        if (f.isDirectory()) {
            return;
        }

        Intent inRet = new Intent();
        inRet.putExtra("path", item.fullPath);
        getActivity().setResult(Activity.RESULT_OK, inRet);
        getActivity().finish();
    }

    @Override
    public void onCanExit() {
        getActivity().finish();
    }
}
