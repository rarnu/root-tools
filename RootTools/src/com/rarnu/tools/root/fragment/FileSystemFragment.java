package com.rarnu.tools.root.fragment;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.EditText;
import android.widget.Toast;
import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.FileOperationInfo;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.component.FileExploreListener;
import com.rarnu.tools.root.component.FileExplorerView;
import com.rarnu.tools.root.fragmentactivity.ChangePermissionActivity;
import com.rarnu.tools.root.fragmentactivity.InstallApkActivity;
import com.rarnu.tools.root.fragmentactivity.PoolActivity;
import com.rarnu.tools.root.fragmentactivity.TextEditorActivity;
import com.rarnu.utils.FileUtils;
import com.rarnu.utils.ImageUtils;
import com.rarnu.utils.common.FileSystemFileInfo;

import java.io.File;

public class FileSystemFragment extends BaseFragment implements FileExploreListener {

    MenuItem itemPool;
    MenuItem itemUp;
    MenuItem itemAdd;
    FileExplorerView fevFiles;

    private Handler hFileHandler = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1 && getActivity() != null) {
                fevFiles.endOperating();
                itemPool.setEnabled(true);
                FileOperationInfo info = (FileOperationInfo) msg.obj;
                removeOperatingFile(info);
                Toast.makeText(getActivity(), msg.arg1 == 0 ? R.string.file_operating_succ : R.string.file_operating_fail, Toast.LENGTH_LONG).show();
                if (msg.arg1 == 0) {
                    fevFiles.refreshDir();
                }
            }
            super.handleMessage(msg);
        }
    };

    @Override
    public int getBarTitle() {
        return R.string.func_filesystem;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.func_filesystem_with_path;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        fevFiles = (FileExplorerView) innerView.findViewById(R.id.fevFiles);

    }

    @Override
    public void initEvents() {
        fevFiles.setFileExploreListener(this);
    }

    @Override
    public void initLogic() {
        fevFiles.setCurrentDir("/");
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_filesystem;
    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
    }

    @Override
    public void initMenu(Menu menu) {
        itemPool = menu.add(0, MenuItemIds.MENU_POOL, 97, R.string.pool);
        itemPool.setIcon(ImageUtils.loadActionBarIcon(getActivity(), GlobalInstance.listOperation.size() == 0 ? R.drawable.fs_gray : R.drawable.fs_light));
        itemPool.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);

        itemUp = menu.add(0, MenuItemIds.MENU_UPLEVEL, 98, R.string.uplevel);
        itemUp.setIcon(ImageUtils.loadActionBarIcon(getActivity(), R.drawable.up_level));
        itemUp.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);

        itemAdd = menu.add(0, MenuItemIds.MENU_ADD, 99, R.string.add);
        itemAdd.setIcon(android.R.drawable.ic_menu_add);
        itemAdd.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);

    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuItemIds.MENU_UPLEVEL:
                fevFiles.doUpLevel();
                break;
            case MenuItemIds.MENU_ADD:
                doPrepareAddFile();
                break;
            case MenuItemIds.MENU_POOL:
                if (GlobalInstance.listOperation.size() != 0) {
                    Intent inPool = new Intent(getActivity(), PoolActivity.class);
                    startActivityForResult(inPool, 0);
                } else {
                    Toast.makeText(getActivity(), R.string.no_file_for_operating, Toast.LENGTH_SHORT).show();
                }
                break;
        }
        return true;
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (resultCode != Activity.RESULT_OK) {
            return;
        }
        switch (requestCode) {
            case 0:
                if (data != null && data.hasExtra("info")) {
                    FileOperationInfo info = (FileOperationInfo) data.getSerializableExtra("info");
                    switch (info.operation) {
                        case 0:
                            doCutFile(info);
                            break;
                        case 1:
                            doCopyFile(info);
                            break;
                    }
                }
                break;
        }
        itemPool.setIcon(ImageUtils.loadActionBarIcon(getActivity(), GlobalInstance.listOperation.size() == 0 ? R.drawable.fs_gray : R.drawable.fs_light));
    }

    private void doPrepareAddFile() {
        // add new file
        final EditText etFileName = new EditText(getActivity());
        new AlertDialog.Builder(getActivity())
                .setTitle(R.string.file_input_name)
                .setView(etFileName)
                .setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        String fileName = etFileName.getText().toString();
                        if (!fileName.equals("")) {
                            doAddFile(fileName);
                        }
                    }
                })
                .setNegativeButton(R.string.cancel, null)
                .show();
    }

    private void doAddFile(String fileName) {
        fevFiles.addFile(fileName);
    }

    private void doCutFile(final FileOperationInfo info) {
        // cut file
        doFileOperating(info, R.string.file_moving, 0);
    }

    private void doCopyFile(final FileOperationInfo info) {
        // copy file
        doFileOperating(info, R.string.file_copying, 1);
    }

    private void doFileOperating(final FileOperationInfo info, final int hintText, final int type) {
        fevFiles.startOperating(hintText);
        itemPool.setEnabled(false);

        new Thread(new Runnable() {
            @Override
            public void run() {

                CommandResult cmdRet = RootUtils.runCommand(String.format("cp -r \"%s\" \"%s/\"", info.fullPath, fevFiles.getCurrentDir()), true);
                if (cmdRet.error.equals("") && type == 0) {
                    cmdRet = RootUtils.runCommand(String.format("rm -r \"%s\"", info.fullPath), true);
                }

                Message msg = new Message();
                msg.what = 1;
                msg.arg1 = cmdRet.error.equals("") ? 0 : 1;
                msg.obj = info;
                hFileHandler.sendMessage(msg);
            }
        }).start();
    }

    private void removeOperatingFile(FileOperationInfo info) {
        for (FileOperationInfo fi : GlobalInstance.listOperation) {
            if (fi.fullPath.equals(info.fullPath)) {
                GlobalInstance.listOperation.remove(fi);
                break;
            }
        }
        itemPool.setIcon(ImageUtils.loadActionBarIcon(getActivity(), GlobalInstance.listOperation.size() == 0 ? R.drawable.fs_gray : R.drawable.fs_light));
    }

    @Override
    public void onGetNewArguments(Bundle bn) {
        fevFiles.doUpLevel();
    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

    private void doOpenAsText(final FileSystemFileInfo item) {
        if (item.isDirectory) {
            Toast.makeText(getActivity(), R.string.file_cannot_open_folder, Toast.LENGTH_SHORT).show();
        } else {

            if (item != null && item.mimeType != null && item.mimeType.startsWith("text/")) {
                startActivity(new Intent(getActivity(), TextEditorActivity.class).putExtra("file", item.fullPath).putExtra("name", item.name));
            } else {
                Toast.makeText(getActivity(), R.string.file_not_text, Toast.LENGTH_SHORT).show();
            }

        }
    }

    private void doPrepareCutFile(final FileSystemFileInfo item) {
        doPrepareFile(item, 0);
    }

    private void doPrepareCopyFile(final FileSystemFileInfo item) {
        doPrepareFile(item, 1);
    }

    private void doPrepareFile(final FileSystemFileInfo item, int operation) {
        if (!isOperationExists(item)) {
            FileOperationInfo info = new FileOperationInfo();
            info.fullPath = item.fullPath;
            info.name = item.name;
            info.operation = operation;
            info.icon = (new File(item.fullPath).isDirectory()) ? R.drawable.format_folder : item.icon;
            GlobalInstance.listOperation.add(info);
            itemPool.setIcon(ImageUtils.loadActionBarIcon(getActivity(), R.drawable.fs_light));
        } else {
            Toast.makeText(getActivity(), R.string.file_operating_exists, Toast.LENGTH_SHORT).show();
        }
    }

    private boolean isOperationExists(FileSystemFileInfo item) {
        boolean ret = false;
        for (FileOperationInfo fi : GlobalInstance.listOperation) {
            if (fi.fullPath.equals(item.fullPath)) {
                ret = true;
                break;
            }
        }
        return ret;
    }

    private void doPrepareDeleteFile(final FileSystemFileInfo item) {
        new AlertDialog.Builder(getActivity())
                .setTitle(R.string.hint)
                .setMessage(R.string.delete_file)
                .setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        if (item.fullPath.startsWith("/system/") || item.fullPath.startsWith("/data/")) {
                            RootUtils.runCommand(String.format("rm -r %s", item.fullPath), true);
                        } else {
                            File fDel = new File(item.fullPath);
                            Log.e("doPrepareDeleteFile", item.fullPath);
                            if (fDel.isDirectory()) {
                                FileUtils.deleteDir(item.fullPath);
                            } else {
                                fDel.delete();
                            }
                        }
                        fevFiles.deleteFile(item);
                    }
                })
                .setNegativeButton(R.string.cancel, null)
                .show();
    }

    private void doPrepareChangePermission(final FileSystemFileInfo item) {
        // change permission
        Intent inPermission = new Intent(getActivity(), ChangePermissionActivity.class);
        inPermission.putExtra("file", item);
        startActivity(inPermission);
    }

    private void doInstall(final FileSystemFileInfo item) {
        doOpenInstallApk(item.fullPath, 0);
    }

    private void doForceInstall(final FileSystemFileInfo item) {
        doOpenInstallApk(item.fullPath, 1);
    }

    private void doOpenInstallApk(final String filePath, int mode) {
        Intent inInstall = new Intent(getActivity(), InstallApkActivity.class);
        inInstall.putExtra("filePath", filePath);
        inInstall.putExtra("mode", mode);
        startActivity(inInstall);
    }

    @Override
    public void onGotoStart() {
        if (itemPool != null) {
            itemPool.setEnabled(false);
        }
        if (itemUp != null) {
            itemUp.setEnabled(false);
        }
    }

    @Override
    public void onGotoEnd() {
        if (itemPool != null) {
            itemPool.setEnabled(true);
        }
        if (itemUp != null) {
            itemUp.setEnabled(true);
        }
    }

    @Override
    public void onFileItemLongClick(final FileSystemFileInfo item) {
        int menu_id = R.array.array_file_system_item_menu;
        if (item.ext.equals(".txt")) {
            menu_id = R.array.array_file_system_item_menu_text;
        } else if (item.ext.equals(".apk")) {
            menu_id = R.array.array_file_system_item_menu_apk;
        }
        final int final_menu_id = menu_id;
        new AlertDialog.Builder(getActivity())
                .setTitle(R.string.filesystem)
                .setItems(menu_id, new DialogInterface.OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        switch (final_menu_id) {
                            case R.array.array_file_system_item_menu:
                                switch (which) {
                                    case 0:
                                        doPrepareCutFile(item);
                                        break;
                                    case 1:
                                        doPrepareCopyFile(item);
                                        break;
                                    case 2:
                                        doPrepareDeleteFile(item);
                                        break;
                                    case 3:
                                        doPrepareChangePermission(item);
                                        break;
                                }
                                break;
                            case R.array.array_file_system_item_menu_text:
                                switch (which) {
                                    case 0:
                                        doOpenAsText(item);
                                        break;
                                    case 1:
                                        doPrepareCutFile(item);
                                        break;
                                    case 2:
                                        doPrepareCopyFile(item);
                                        break;
                                    case 3:
                                        doPrepareDeleteFile(item);
                                        break;
                                    case 4:
                                        doPrepareChangePermission(item);
                                        break;
                                }
                                break;
                            case R.array.array_file_system_item_menu_apk:
                                switch (which) {
                                    case 0:
                                        doInstall(item);
                                        break;
                                    case 1:
                                        doForceInstall(item);
                                        break;
                                    case 2:
                                        doPrepareCutFile(item);
                                        break;
                                    case 3:
                                        doPrepareCopyFile(item);
                                        break;
                                    case 4:
                                        doPrepareDeleteFile(item);
                                        break;
                                    case 5:
                                        doPrepareChangePermission(item);
                                        break;
                                }
                                break;
                        }

                    }
                })
                .setNegativeButton(R.string.cancel, null)
                .show();
    }

    @Override
    public void onCanExit() {
        getActivity().finish();
    }
}
