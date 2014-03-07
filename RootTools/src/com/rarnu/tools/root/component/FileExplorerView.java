package com.rarnu.tools.root.component;

import android.content.Context;
import android.os.Handler;
import android.os.Message;
import android.util.AttributeSet;
import android.view.Gravity;
import android.view.View;
import android.widget.*;
import com.rarnu.devlib.component.DataProgressBar;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.FileSystemAdapter;
import com.rarnu.utils.FileCommandUtils;
import com.rarnu.utils.FileUtils;
import com.rarnu.utils.UIUtils;
import com.rarnu.utils.common.FileSystemFileInfo;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

public class FileExplorerView extends RelativeLayout implements SearchView.OnQueryTextListener, AdapterView.OnItemClickListener, AdapterView.OnItemLongClickListener {

    private static final int ID_SEARCHVIEW = 100001;
    private static final int ID_TEXTVIEW = 100002;
    private static final int ID_LISTVIEW = 100003;
    private static final int ID_PROGRESSBAR = 100004;

    SearchView sv;
    TextView tvPath;
    ListView lvFiles;
    DataProgressBar progressFiles;

    String currentDir = "/";
    boolean canExit = false;
    String fileType = "";

    FileExploreListener listener = null;

    List<FileSystemFileInfo> list;
    private Handler hShowFiles = new Handler() {

        @SuppressWarnings("unchecked")
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1 && getContext() != null) {
                list = (List<FileSystemFileInfo>) msg.obj;
                adapter.setNewList(list);
                lvFiles.setEnabled(true);
                progressFiles.setVisibility(View.GONE);

                if (listener != null) {
                    listener.onGotoEnd();
                }

            }
            super.handleMessage(msg);
        }
    };
    FileSystemAdapter adapter;
    private Comparator<FileSystemFileInfo> compFiles = new Comparator<FileSystemFileInfo>() {
        @Override
        public int compare(FileSystemFileInfo lhs, FileSystemFileInfo rhs) {
            return lhs.name.compareTo(rhs.name);
        }
    };

    public FileExplorerView(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        init();
    }

    public FileExplorerView(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    public FileExplorerView(Context context) {
        super(context);
        init();
    }

    public void setFileExploreListener(FileExploreListener listener) {
        this.listener = listener;
    }

    private void init() {
        sv = new SearchView(getContext());
        sv.setId(ID_SEARCHVIEW);
        sv.setFocusable(false);
        sv.setIconifiedByDefault(false);
        sv.setLayoutParams(new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT));

        tvPath = new TextView(getContext());
        tvPath.setId(ID_TEXTVIEW);
        tvPath.setBackgroundColor(getContext().getResources().getColor(R.color.lightskyblue));
        tvPath.setTextColor(getContext().getResources().getColor(R.color.black));
        int padding = UIUtils.dipToPx(4);
        tvPath.setPadding(padding, padding, padding, padding);
        tvPath.setGravity(Gravity.LEFT | Gravity.CENTER_VERTICAL);
        LayoutParams lpPath = new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT);
        lpPath.addRule(RelativeLayout.BELOW, ID_SEARCHVIEW);
        tvPath.setLayoutParams(lpPath);

        progressFiles = new DataProgressBar(getContext());
        progressFiles.setId(ID_PROGRESSBAR);
        progressFiles.setVisibility(View.GONE);
        LayoutParams lpProgress = new LayoutParams(LayoutParams.MATCH_PARENT, UIUtils.dipToPx(24));
        lpProgress.addRule(RelativeLayout.ALIGN_PARENT_BOTTOM, RelativeLayout.TRUE);
        progressFiles.setLayoutParams(lpProgress);

        lvFiles = new ListView(getContext());
        lvFiles.setId(ID_LISTVIEW);
        lvFiles.setFastScrollEnabled(true);
        LayoutParams lpListview = new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT);
        lpListview.addRule(RelativeLayout.ABOVE, ID_PROGRESSBAR);
        lpListview.addRule(RelativeLayout.BELOW, ID_TEXTVIEW);
        lvFiles.setLayoutParams(lpListview);

        addView(sv);
        addView(tvPath);
        addView(progressFiles);
        addView(lvFiles);

        list = new ArrayList<FileSystemFileInfo>();
        adapter = new FileSystemAdapter(getContext(), list);
        lvFiles.setAdapter(adapter);
        lvFiles.setOnItemClickListener(this);
        lvFiles.setOnItemLongClickListener(this);
        sv.setOnQueryTextListener(this);

    }

    public void doUpLevel() {
        if (!currentDir.equals("/")) {
            currentDir = currentDir.substring(0, currentDir.lastIndexOf("/"));
            if (currentDir.equals("")) {
                currentDir = "/";
            }
            gotoDir(currentDir);
        } else {
            if (canExit) {

                if (listener != null) {
                    listener.onCanExit();
                }
            } else {
                canExit = true;
                Toast.makeText(getContext(), R.string.already_sdcard_root, Toast.LENGTH_SHORT).show();
            }
        }

    }

    public void refreshDir() {
        gotoDir(currentDir);
    }

    public void gotoDir(String dir) {
        canExit = false;
        if (listener != null) {
            listener.onGotoStart();
        }

        lvFiles.setEnabled(false);
        progressFiles.setAppName(getContext().getString(R.string.loading));
        progressFiles.setVisibility(View.VISIBLE);

        currentDir = dir;
        tvPath.setText(currentDir);
        new Thread(new Runnable() {
            @Override
            public void run() {
                List<FileSystemFileInfo> listTmp = FileCommandUtils.getFileList(currentDir, fileType);
                Collections.sort(listTmp, compFiles);
                Message msg = new Message();
                msg.what = 1;
                msg.obj = listTmp;
                hShowFiles.sendMessage(msg);
            }
        }).start();
    }

    @Override
    public boolean onQueryTextChange(String newText) {
        if (adapter != null) {
            adapter.getFilter().filter(newText);
        }
        return false;
    }

    @Override
    public boolean onQueryTextSubmit(String query) {
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

    @Override
    public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {
        final FileSystemFileInfo item = (FileSystemFileInfo) lvFiles.getItemAtPosition(position);
        if (listener != null) {
            listener.onFileItemLongClick(item);
        }
        return true;
    }

    public void deleteFile(FileSystemFileInfo item) {
        list.remove(item);
        adapter.setNewList(list);
        if (sv != null) {
            adapter.filter(sv.getQuery().toString());
        }
    }

    public void addFile(String fileName) {
        String fullPath = currentDir + "/" + fileName;
        try {
            FileUtils.rewriteFile(fullPath, "");
            FileSystemFileInfo info = new FileSystemFileInfo(fileName, fullPath);
            info.icon = R.drawable.format_file;
            list.add(info);
            adapter.setNewList(list);
        } catch (Exception e) {
        }
    }

    public void startOperating(int hint) {
        progressFiles.setAppName(getContext().getString(hint));
        progressFiles.setVisibility(View.VISIBLE);
        lvFiles.setEnabled(false);
    }

    public void endOperating() {
        lvFiles.setEnabled(true);
        progressFiles.setVisibility(View.GONE);
    }

    public String getCurrentDir() {
        return currentDir;
    }

    public void setCurrentDir(String dir) {
        currentDir = dir;
        refreshDir();
    }

    public void setFileType(String type) {
        this.fileType = type;
    }
}
