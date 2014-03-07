package com.rarnu.tools.root.component;

import com.rarnu.utils.common.FileSystemFileInfo;

public interface FileExploreListener {
    void onGotoStart();
    void onGotoEnd();
    void onFileItemLongClick(FileSystemFileInfo item);
    void onCanExit();
}
