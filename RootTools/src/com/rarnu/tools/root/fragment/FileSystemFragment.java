package com.rarnu.tools.root.fragment;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.*;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.SearchView.OnQueryTextListener;
import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.DataProgressBar;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.FileSystemAdapter;
import com.rarnu.tools.root.common.FileOperationInfo;
import com.rarnu.tools.root.common.FileSystemFileInfo;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.fragmentactivity.PoolActivity;
import com.rarnu.tools.root.fragmentactivity.TextEditorActivity;
import com.rarnu.utils.FileUtils;
import com.rarnu.utils.ImageUtils;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

public class FileSystemFragment extends BaseFragment implements OnQueryTextListener, OnItemClickListener, AdapterView.OnItemLongClickListener {

    MenuItem itemPool;
    MenuItem itemSearch;
    MenuItem itemUp;
    MenuItem itemAdd;
    TextView tvPath;
    ListView lvFiles;
    DataProgressBar progressFiles;
    String currentDir = "/";
    List<FileSystemFileInfo> list;
    FileSystemAdapter adapter;
    boolean canExit = false;
    private Handler hShowFiles = new Handler() {

        @SuppressWarnings("unchecked")
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                list = (List<FileSystemFileInfo>) msg.obj;
                adapter.setNewList(list);
                lvFiles.setEnabled(true);
                if (itemPool != null) {
                    itemPool.setEnabled(true);
                }
                if (itemUp != null) {
                    itemUp.setEnabled(true);
                }
                progressFiles.setVisibility(View.GONE);
            }
            super.handleMessage(msg);
        }
    };
    private Comparator<FileSystemFileInfo> compFiles = new Comparator<FileSystemFileInfo>() {
        @Override
        public int compare(FileSystemFileInfo lhs, FileSystemFileInfo rhs) {
            return lhs.name.compareTo(rhs.name);
        }
    };
    private Handler hFileHandler = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                lvFiles.setEnabled(true);
                itemPool.setEnabled(true);
                progressFiles.setVisibility(View.GONE);
                FileOperationInfo info = (FileOperationInfo) msg.obj;
                removeOperatingFile(info);
                Toast.makeText(getActivity(), msg.arg1 == 0 ? R.string.file_operating_succ : R.string.file_operating_fail, Toast.LENGTH_LONG).show();
                if (msg.arg1 == 0) {
                    gotoDir(currentDir);
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
        lvFiles = (ListView) innerView.findViewById(R.id.lvFiles);
        progressFiles = (DataProgressBar) innerView.findViewById(R.id.progressFiles);
        tvPath = (TextView) innerView.findViewById(R.id.tvPath);

        list = new ArrayList<FileSystemFileInfo>();
        adapter = new FileSystemAdapter(getActivity(), list);
        lvFiles.setAdapter(adapter);

    }

    @Override
    public void initEvents() {
        lvFiles.setOnItemClickListener(this);
        lvFiles.setOnItemLongClickListener(this);
    }

    @Override
    public void initLogic() {
        gotoDir(currentDir);
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
        itemSearch = menu.add(0, MenuItemIds.MENU_SEARCH, 97, R.string.search);
        itemSearch.setIcon(android.R.drawable.ic_menu_search);
        itemSearch.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
        SearchView sv = new SearchView(getActivity());
        sv.setOnQueryTextListener(this);
        itemSearch.setActionView(sv);

        itemUp = menu.add(0, MenuItemIds.MENU_UPLEVEL, 98, R.string.uplevel);
        itemUp.setIcon(ImageUtils.loadActionBarIcon(getActivity(), R.drawable.up_level));
        itemUp.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);

        itemAdd = menu.add(0, MenuItemIds.MENU_ADD, 99, R.string.add);
        itemAdd.setIcon(android.R.drawable.ic_menu_add);
        itemAdd.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);

        itemPool = menu.add(0, MenuItemIds.MENU_POOL, 100, R.string.pool);
        itemPool.setIcon(ImageUtils.loadActionBarIcon(getActivity(), GlobalInstance.listOperation.size() == 0 ? R.drawable.fs_gray : R.drawable.fs_light));
        itemPool.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuItemIds.MENU_UPLEVEL:
                doUpLevel();
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
        String fullPath = currentDir + "/" + fileName;
        try {
            FileUtils.rewriteFile(fullPath, "");
            FileSystemFileInfo info = new FileSystemFileInfo(false, fileName, fullPath);
            // TODO: fill icon
            list.add(info);
            adapter.setNewList(list);
        } catch (Exception e) {
        }
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
        progressFiles.setAppName(getString(hintText));
        progressFiles.setVisibility(View.VISIBLE);
        lvFiles.setEnabled(false);
        itemPool.setEnabled(false);

        new Thread(new Runnable() {
            @Override
            public void run() {

                CommandResult cmdRet = RootUtils.runCommand(String.format("cp -r \"%s\" \"%s/\"", info.fullPath, currentDir), true);
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
        for (int i = 0; i < GlobalInstance.listOperation.size(); i++) {
            if (GlobalInstance.listOperation.get(i).fullPath.equals(info.fullPath)) {
                GlobalInstance.listOperation.remove(i);
                break;
            }
        }
        itemPool.setIcon(ImageUtils.loadActionBarIcon(getActivity(), GlobalInstance.listOperation.size() == 0 ? R.drawable.fs_gray : R.drawable.fs_light));
    }

    private void doUpLevel() {
        if (!currentDir.equals("/")) {
            currentDir = currentDir.substring(0, currentDir.lastIndexOf("/"));
            if (currentDir.equals("")) {
                currentDir = "/";
            }
            gotoDir(currentDir);
        } else {
            if (canExit) {
                getActivity().finish();
            } else {
                canExit = true;
                Toast.makeText(getActivity(), R.string.already_sdcard_root, Toast.LENGTH_SHORT).show();
            }
        }

    }

    @Override
    public void onGetNewArguments(Bundle bn) {
        doUpLevel();
    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

    @Override
    public boolean onQueryTextSubmit(String query) {
        return false;
    }

    @Override
    public boolean onQueryTextChange(String newText) {
        adapter.getFilter().filter(newText);
        return false;
    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        FileSystemFileInfo item = (FileSystemFileInfo) lvFiles.getItemAtPosition(position);
        if (item.isDirectory) {
            if (!currentDir.endsWith("/")) {
                currentDir += "/";
            }
            currentDir += item.name;
            gotoDir(currentDir);
        }
    }

    private void gotoDir(String dir) {
        canExit = false;
        lvFiles.setEnabled(false);
        if (itemPool != null) {
            itemPool.setEnabled(false);
        }
        if (itemUp != null) {
            itemUp.setEnabled(false);
        }
        progressFiles.setAppName(getString(R.string.loading));
        progressFiles.setVisibility(View.VISIBLE);

        currentDir = dir;
        tvPath.setText(currentDir);
        new Thread(new Runnable() {
            @Override
            public void run() {
                List<FileSystemFileInfo> listTmp = new ArrayList<FileSystemFileInfo>();
                try {
                    File fDir = new File(currentDir);
                    File[] files = fDir.listFiles();
                    for (int i = 0; i < files.length; i++) {
                        FileSystemFileInfo info = new FileSystemFileInfo(files[i].isDirectory(), files[i].getName(), files[i].getAbsolutePath());
                        info.icon = getIconResForFile(files[i].getName());
                        listTmp.add(info);
                    }

                    Collections.sort(listTmp, compFiles);
                } catch (Throwable th) {

                }
                Message msg = new Message();
                msg.what = 1;
                msg.obj = listTmp;
                hShowFiles.sendMessage(msg);
            }
        }).start();
    }

    private int getIconResForFile(String fileName) {
        int ret = R.drawable.format_file;
        fileName = fileName.toLowerCase();
        if (fileName.endsWith("apk")) {
            ret = R.drawable.format_apk;
        } else if (fileName.endsWith("chm")) {
            ret = R.drawable.format_chm;
        } else if (fileName.endsWith("doc") || fileName.endsWith("docx")) {
            ret = R.drawable.format_word;
        } else if (fileName.endsWith("xls") || fileName.endsWith("xlsx")) {
            ret = R.drawable.format_excel;
        } else if (fileName.endsWith("ppt") || fileName.endsWith("pptx")) {
            ret = R.drawable.format_ppt;
        } else if (fileName.endsWith("txt") || fileName.endsWith("rtf")) {
            ret = R.drawable.format_text;
        } else if (fileName.endsWith("zip") || fileName.endsWith("rar") || fileName.endsWith("tar") || fileName.endsWith("gz") || fileName.endsWith("bz") || fileName.endsWith("bz2") || fileName.endsWith("jar")) {
            ret = R.drawable.format_zip;
        } else if (fileName.endsWith("png") || fileName.endsWith("jpg") || fileName.endsWith("bmp") || fileName.endsWith("gif") || fileName.endsWith("webp") || fileName.endsWith("jpeg") || fileName.endsWith("ico")) {
            ret = R.drawable.format_picture;
        } else if (fileName.endsWith("pdf")) {
            ret = R.drawable.format_pdf;
        } else if (fileName.endsWith("mp3") || fileName.endsWith("ogg") || fileName.endsWith("wav") || fileName.endsWith("wma")) {
            ret = R.drawable.format_music;
        } else if (fileName.endsWith("avi") || fileName.endsWith("rm") || fileName.endsWith("rmvb") || fileName.endsWith("mp4") || fileName.endsWith("3gp") || fileName.endsWith("wmv") || fileName.endsWith("mpg")) {
            ret = R.drawable.format_media;
        } else if (fileName.endsWith("swf") || fileName.endsWith("flv") || fileName.endsWith("f4v")) {
            ret = R.drawable.format_flash;
        } else if (fileName.endsWith("htm") || fileName.endsWith("html") || fileName.endsWith("xhtml")) {
            ret = R.drawable.format_html;
        }
        return ret;
    }

    @Override
    public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {
        final FileSystemFileInfo item = (FileSystemFileInfo) lvFiles.getItemAtPosition(position);
        new AlertDialog.Builder(getActivity())
                .setTitle(R.string.filesystem)
                .setItems(R.array.array_file_system_item_menu, new DialogInterface.OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialog, int which) {
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
                        }
                    }
                })
                .setNegativeButton(R.string.cancel, null)
                .show();

        return true;
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
        for (int i = 0; i < GlobalInstance.listOperation.size(); i++) {
            if (GlobalInstance.listOperation.get(i).fullPath.equals(item.fullPath)) {
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
                        FileUtils.deleteFile(item.fullPath);
                        list.remove(item);
                        adapter.setNewList(list);
                        if (itemSearch != null) {
                            SearchView sv = (SearchView) itemSearch.getActionView();
                            adapter.filter(sv.getQuery().toString());
                        }
                    }
                })
                .setNegativeButton(R.string.cancel, null)
                .show();
    }
}
