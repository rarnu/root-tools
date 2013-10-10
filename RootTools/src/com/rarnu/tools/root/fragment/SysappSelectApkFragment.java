package com.rarnu.tools.root.fragment;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.os.Environment;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.*;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.SearchView.OnQueryTextListener;
import com.rarnu.devlib.base.BasePopupFragment;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.SysappSelectApkAdapter;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.common.SysappSelectApkItem;
import com.rarnu.tools.root.utils.ApkUtils;
import com.rarnu.utils.ImageUtils;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

public class SysappSelectApkFragment extends BasePopupFragment implements OnItemClickListener, OnQueryTextListener {

    public static String ApkFilePath = "";
    private static String rootDir = Environment.getExternalStorageDirectory().getAbsolutePath();
    final Handler hShowDir = new Handler() {

        @SuppressWarnings("unchecked")
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                list = (List<SysappSelectApkItem>) msg.obj;
                adapter.setNewList(list);
                pbShowing.setVisibility(View.GONE);
                lvFiles.setEnabled(true);
            }
            super.handleMessage(msg);
        }
    };
    SysappSelectApkAdapter adapter = null;
    String currentDir = rootDir;
    boolean canExit = false;
    ListView lvFiles;
    TextView tvPath;
    ProgressBar pbShowing;
    MenuItem itemSearch;
    MenuItem itemUp;
    List<SysappSelectApkItem> list;
    private Comparator<SysappSelectApkItem> compApks = new Comparator<SysappSelectApkItem>() {
        @Override
        public int compare(SysappSelectApkItem lhs, SysappSelectApkItem rhs) {
            return lhs.filename.compareTo(rhs.filename);
        }
    };

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
        lvFiles = (ListView) innerView.findViewById(R.id.lvApk);
        tvPath = (TextView) innerView.findViewById(R.id.tvPath);
        pbShowing = (ProgressBar) innerView.findViewById(R.id.pbShowing);
        list = new ArrayList<SysappSelectApkItem>();
        adapter = new SysappSelectApkAdapter(getActivity(), list);
        lvFiles.setAdapter(adapter);
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_sysapp_selectapk;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuItemIds.MENU_UPLEVEL:
                doUplevel();
                break;
        }
        return true;
    }

    private void doUplevel() {
        if (!currentDir.equals(rootDir)) {
            currentDir = currentDir.substring(0, currentDir.lastIndexOf("/"));
            showDirT(currentDir);
        } else {
            if (canExit) {
                getActivity().finish();
            } else {
                canExit = true;
                Toast.makeText(getActivity(), R.string.already_sdcard_root, Toast.LENGTH_SHORT).show();
            }
        }
    }

    public List<SysappSelectApkItem> showDir(String dir) {
        List<SysappSelectApkItem> listTemp = new ArrayList<SysappSelectApkItem>();
        try {
            File fDir = new File(dir);
            if (fDir.exists()) {
                File[] files = fDir.listFiles();
                if (files != null) {

                    for (File f : files) {
                        if (!f.getName().startsWith(".")) {
                            if (f.isDirectory() || f.getName().endsWith(".apk")) {
                                SysappSelectApkItem item = new SysappSelectApkItem();
                                if (!f.isDirectory()) {
                                    item.iconImg = ApkUtils.getIconFromPackage(getActivity(), f.getAbsolutePath());
                                }
                                item.icon = f.isDirectory() ? 1 : 0;
                                item.filename = f.getName();
                                item.level = ApkUtils.getAppLevel(f.getAbsolutePath(), "");
                                listTemp.add(item);
                            }
                        }
                    }
                }
            }
        } catch (Throwable th) {

        }
        if (listTemp != null) {
            Collections.sort(listTemp, compApks);
        }
        return listTemp;
    }

    public void showDirT(final String dir) {
        canExit = false;
        pbShowing.setVisibility(View.VISIBLE);
        tvPath.setText(dir);
        lvFiles.setEnabled(false);
        new Thread(new Runnable() {
            @Override
            public void run() {
                Message msg = new Message();
                msg.what = 1;
                msg.obj = showDir(dir);
                hShowDir.sendMessage(msg);

            }
        }).start();
    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        SysappSelectApkItem item = (SysappSelectApkItem) lvFiles.getItemAtPosition(position);
        File f = new File(currentDir + "/" + item.filename);
        if (f.isDirectory()) {
            currentDir = currentDir + "/" + item.filename;
            showDirT(currentDir);
            return;
        }
        Intent inRet = new Intent();
        inRet.putExtra("path", f.getAbsolutePath());
        getActivity().setResult(Activity.RESULT_OK, inRet);
        getActivity().finish();
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
    public void initMenu(Menu menu) {
        itemSearch = menu.add(0, MenuItemIds.MENU_SEARCH, 98, R.string.search);
        itemSearch.setIcon(android.R.drawable.ic_menu_search);
        itemSearch.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
        SearchView sv = new SearchView(getActivity());
        sv.setOnQueryTextListener(this);
        itemSearch.setActionView(sv);

        itemUp = menu.add(0, MenuItemIds.MENU_UPLEVEL, 99, R.string.uplevel);
        itemUp.setIcon(ImageUtils.loadActionBarIcon(getActivity(), R.drawable.up_level));
        itemUp.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);

    }

    @Override
    public void initLogic() {
        showDirT(currentDir);
    }

    @Override
    public void initEvents() {
        lvFiles.setOnItemClickListener(this);
    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
    }

    @Override
    public void onGetNewArguments(Bundle bn) {
        doUplevel();
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

}
