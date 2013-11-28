package com.rarnu.tools.root.loader;

import android.content.Context;
import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.tools.root.common.DataappInfo;
import com.rarnu.tools.root.utils.BackupRestoreUtils;

import java.util.List;

public class RestoreLoader extends BaseLoader<DataappInfo> {

    private String path;

    public RestoreLoader(Context context) {
        super(context);
    }

    public void setPath(String path) {
        this.path = path;
    }

    @Override
    public List<DataappInfo> loadInBackground() {
        return BackupRestoreUtils.getBackupedApps(getContext(), path);
    }

}
